{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Command
import Sos

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.ByteString        (ByteString)
import Data.Monoid
import Data.Yaml              (decodeFileEither, prettyPrintParseException)
import Prelude                hiding (FilePath)
import Options.Applicative
import System.Console.ANSI
import System.Console.Concurrent
import System.Directory
import System.Exit
import System.FilePath
import System.FSNotify
import System.Process
import Text.Regex.TDFA

import qualified Data.ByteString.Char8 as BS

version :: String
version = "Steel Overseer 2.0"

data Options = Options
    { optTarget   :: FilePath
    , optCommands :: [ByteString]
    , optPatterns :: [ByteString]
    } deriving Show

-- A currently running command.
type RunningCommand =
    ( MVar (Async ()) -- Currently running command (might be finished)
    , CommandPlan
    )

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

    cwd <- getCurrentDirectory

    outputConcurrentLn "Hit enter to quit."

    withManager $ \wm -> do
        runningCmds <- mapM (\plan -> do
                                mv <- newMVar =<< async (pure ())
                                pure (mv, plan))
                            plans

        let predicate = \event -> or (map (\plan -> match (cmdRegex plan) (eventPath event)) plans)
        _ <- watchTree wm target predicate $ \event -> do
            outputConcurrentLn ("\n" <> colored Cyan (showEvent event cwd))
            mapM_ (handleEvent event cwd) runningCmds

        _ <- getLine
        mapM_ (\(mv, _) -> takeMVar mv >>= cancel) runningCmds

handleEvent :: Event -> FilePath -> RunningCommand -> IO ()
handleEvent event cwd (cmdThread, CommandPlan{..}) = do
    let path = BS.pack (makeRelative cwd (eventPath event))
    case match cmdRegex path of
        [] -> pure ()
        (captures:_) -> do
            commands <- runSos (mapM (\t -> instantiateTemplate t captures) cmdTemplates)

            case commands of
                [] -> pure ()
                _ -> do
                    a <- modifyMVar cmdThread $ \old_a -> do
                        cancel old_a
                        new_a <- async (runCommands commands)
                        pure (new_a, new_a)
                    _ <- waitCatch a
                    pure ()

runCommands :: [String] -> IO ()
runCommands [] = pure ()
runCommands (cmd:cmds) = do
    success <- bracketOnError (runCommand cmd) (\ph -> terminateProcess ph >> pure False) $ \ph -> do
        outputConcurrentLn (colored Magenta "\n> " <> cmd)

        exitCode <- waitForProcess ph
        case exitCode of
            ExitSuccess -> do
                outputConcurrentLn (colored Green "Success ✓")
                pure True
            _ -> do
                outputConcurrentLn (colored Red "Failure ✗")
                pure False
    when success (runCommands cmds)

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
                Right raw_plans -> do
                    plans <- runSos (mapM buildRawCommandPlan raw_plans)
                    putStrLn ("Found " ++ show (length plans) ++ " commands in .sosrc")
                    pure plans
        else pure []

--------------------------------------------------------------------------------

colored :: Color -> String -> String
colored c s = color c <> s <> reset

color :: Color -> String
color c = setSGRCode [SetColor Foreground Dull c]

reset :: String
reset = setSGRCode [Reset]

outputConcurrentLn :: String -> IO ()
outputConcurrentLn s = outputConcurrent (s <> "\n")

showEvent :: Event -> FilePath -> String
showEvent (Added fp _)    cwd = "Added "    ++ makeRelative cwd fp
showEvent (Modified fp _) cwd = "Modified " ++ makeRelative cwd fp
showEvent (Removed fp _)  cwd = "Removed "  ++ makeRelative cwd fp
