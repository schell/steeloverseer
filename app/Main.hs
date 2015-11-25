{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Command
import Sos

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString           (ByteString)
import Data.Monoid
import Data.Sequence             (Seq, ViewL(..), (|>), viewl)
import Data.Yaml                 (decodeFileEither, prettyPrintParseException)
import Prelude                   hiding (FilePath)
import Options.Applicative
import System.Console.ANSI
import System.Directory
import System.Exit
import System.FilePath
import System.FSNotify
import System.Process
import Text.Printf
import Text.Regex.TDFA

import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence         as Seq

version :: String
version = "Steel Overseer 2.0"

data Options = Options
    { optTarget   :: FilePath
    , optCommands :: [ByteString]
    , optPatterns :: [ByteString]
    } deriving Show

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> optsParser)
        ( fullDesc
       <> progDesc "A file watcher and development tool."
       <> header version )

    optsParser :: Parser Options
    optsParser = Options
        <$> strArgument
            ( help "File or directory to watch for changes."
           <> metavar "TARGET" )
        <*> many (fmap BS.pack (strOption
            ( long "command"
           <> short 'c'
           <> help "Add command to run on file event. (default: inferred)"
           <> metavar "COMMAND" )))
        <*> many (fmap BS.pack (strOption
            ( long "pattern"
           <> short 'p'
           <> help "Add pattern to match on file path. Only relevant if the target is a directory. (default: .*)"
           <> metavar "PATTERN" )))

main' :: Options -> IO ()
main' Options{..} = do
    -- Parse .sosrc command plans.
    rc_plans <- parseSosrc

    -- Parse cli commands, where one command is created per pattern that
    -- executes each of the specified sub-commands sequentially.
    cli_plans <- do
        let patterns =
                case (rc_plans, optPatterns) of
                    -- If there are no commands in .sosrc, and no patterns
                    -- specified on the command line, default to ".*"
                    ([], []) -> [".*"]
                    _ -> optPatterns
        runSos (mapM (\pattern -> buildCommandPlan pattern optCommands) patterns)

    (target, plans) <- do
        is_dir  <- doesDirectoryExist optTarget
        is_file <- doesFileExist optTarget
        case (is_dir, is_file) of
            (True, _) -> pure (optTarget, cli_plans ++ rc_plans)
            -- If the target is a single file, completely ignore the .sosrc
            -- commands and the cli commands.
            (_, True) -> do
                plan <- runSos (buildCommandPlan (BS.pack optTarget) optCommands)
                pure (takeDirectory optTarget, [plan])
            _ -> do
                putStrLn ("Target " ++ optTarget ++ " is not a file or directory.")
                exitFailure

    -- Queue of commands to run. The next list of commands to run is at the head
    -- of the list.
    cmds_queue_tvar <- newTVarIO (Seq.empty)

    cwd <- getCurrentDirectory

    putStrLn "Hit Ctrl+C to quit."

    let eventRelPath :: Event -> FilePath
        eventRelPath = \event -> makeRelative cwd (eventPath event)

        predicate :: Event -> Bool
        predicate = \event -> or (map (\plan -> match (cmdRegex plan) (eventRelPath event)) plans)

    withManager $ \wm -> do
        _ <- watchTree wm target predicate $ \event -> do
            -- putStrLn ("\n" <> colored Cyan (showEvent event cwd))
            let path = BS.pack (eventRelPath event)

            commands <- concat <$> mapM (maybeInstantiateTemplate path) plans
            when (commands /= [])
                (atomically (enqueueCommands cmds_queue_tvar (showEvent event cwd) commands))

        forever $ do
            (event, cmds, tmvar) <- atomically $ do
                cmds_queue <- readTVar cmds_queue_tvar
                case viewl cmds_queue of
                    EmptyL -> retry
                    ((event0, cmds0, tmvar0) :< _) -> do
                        -- Clear the TMVar which might have been set before we've even
                        -- started running the commands.
                        _ <- tryTakeTMVar tmvar0
                        pure (event0, cmds0, tmvar0)

            -- Spawn a thread to run the commands, then wait on either that run to
            -- finish *or* the same original TMVar to be signaled (so, a particular
            -- pipeline of commands can only "interrupt" itself - all other concurrently
            -- scheduled commands are simply enqueued.
            let loop = do
                    putStrLn ("\n" <> colored Cyan event)
                    a <- async (runCommands cmds)

                    result <- atomically $
                        (do
                            pollSTM a >>= \case
                                Nothing -> retry
                                Just r -> do
                                    -- Partial pattern matching is safe here because we're the
                                    -- only thread that removes from the command queue, and we
                                    -- just read a non-empty sequence from it above.
                                    modifyTVar' cmds_queue_tvar
                                        (\s -> case viewl s of
                                                   (_ :< rest) -> rest)
                                    pure (Left r))
                        <|> Right <$> takeTMVar tmvar

                    case result of
                        -- Commands threw an exception - a ThreadKilled we are
                        -- okay with, but anything else we should print to the
                        -- console.
                        Left (Left ex) -> do
                            case fromException ex of
                                Just ThreadKilled -> pure ()
                                _ -> putStrLn (colored Red ("Exception: " ++ show ex))

                        -- Commands finished - go back to outer loop.
                        Left (Right ()) -> pure ()

                        -- Another filesystem event triggered this list of commands
                        -- to be run again - cancel the previous run and start over.
                        Right () -> do
                            cancel a
                            _ <- waitCatch a
                            loop
            loop

        pure ()

maybeInstantiateTemplate :: ByteString -> CommandPlan -> IO [Command]
maybeInstantiateTemplate path CommandPlan{..} =
    case match cmdRegex path of
        [] -> pure []
        (captures:_) -> runSos (mapM (instantiateTemplate captures) cmdTemplates)

type EnqueuedCommands = (String, [Command], TMVar ())
type CommandsQueue = Seq EnqueuedCommands

-- | Walk the commands queue - if we see we've already been enqueued, signal the
-- corresponding TMVar. Otherwise, enqueue us at the end.
enqueueCommands :: TVar CommandsQueue -> String -> [Command] -> STM ()
enqueueCommands cmds_queue_tvar event commands =
    let loop :: CommandsQueue -> STM ()
        loop cmds_queue =
            case viewl cmds_queue of
                EmptyL -> do
                    tmvar <- newEmptyTMVar
                    modifyTVar' cmds_queue_tvar (\s -> s |> (event, commands, tmvar))

                (_, commands', tmvar) :< cmds_queue_tail -> do
                    if commands == commands'
                        then void (tryPutTMVar tmvar ())
                        else loop cmds_queue_tail

    in readTVar cmds_queue_tvar >>= loop

-- Run a list of shell commands sequentially. If a command returns ExitFailure,
-- don't run the rest.
runCommands :: [Command] -> IO ()
runCommands cmds0 = go 1 cmds0
  where
    go :: Int -> [Command] -> IO ()
    go _ [] = pure ()
    go n (cmd:cmds) = do
        putStrLn (colored Magenta (printf "[%d/%d] " n (length cmds0)) <> cmd)

        let create = (\(_, _, _, ph) -> ph) <$> createProcess (shell cmd)
            teardown ph = do
                putStrLn (colored Yellow ("Canceling: " ++ cmd))
                terminateProcess ph
        bracketOnError create teardown waitForProcess >>= \case
            ExitSuccess -> do
                putStrLn (colored Green "Success ✓")
                go (n+1) cmds
            _ ->
                putStrLn (colored Red "Failure ✗")

--------------------------------------------------------------------------------

-- Parse a list of command plans from .sosrc.
parseSosrc :: IO [CommandPlan]
parseSosrc = do
    exists <- doesFileExist ".sosrc"
    if exists
        then
            decodeFileEither ".sosrc" >>= \case
                Left err -> do
                    putStrLn ("Error parsing .sosrc:\n" ++ prettyPrintParseException err)
                    exitFailure
                Right (raw_plans :: [RawCommandPlan]) -> do
                    plans <- runSos (mapM buildRawCommandPlan raw_plans)
                    putStrLn "Found .sosrc"
                    pure (concat plans)
        else pure []

--------------------------------------------------------------------------------

colored :: Color -> String -> String
colored c s = color c <> s <> reset

color :: Color -> String
color c = setSGRCode [SetColor Foreground Dull c]

reset :: String
reset = setSGRCode [Reset]

showEvent :: Event -> FilePath -> String
showEvent (Added fp _)    cwd = "Added: "    ++ makeRelative cwd fp
showEvent (Modified fp _) cwd = "Modified: " ++ makeRelative cwd fp
showEvent (Removed fp _)  cwd = "Removed: "  ++ makeRelative cwd fp
