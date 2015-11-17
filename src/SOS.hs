{-# LANGUAGE LambdaCase #-}

module SOS where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Text.Regex.TDFA
import Prelude hiding (FilePath)
import System.Console.ANSI
import System.Exit
import System.FilePath
import System.FSNotify
import System.Process

type Command = String
type Pattern = String

-- | Starts sos watching `target`, matching `ptns` to execute `cmds`.
steelOverseer
    :: FilePath
    -> [Command]
    -> [Regex]
    -> (WatchManager -> FilePath -> ActionPredicate -> Action -> IO StopListening)
    -> IO ()
steelOverseer target cmds ptns watch_fn = do
    putStrLn "Hit enter to quit."
    wm <- startManager

    mvar <- newMVar =<< async (pure ())

    let predicate = \event -> or (map (\ptn -> match ptn (eventPath event)) ptns)
    _ <- watch_fn wm target predicate (performCommand mvar cmds)

    _ <- getLine
    takeMVar mvar >>= cancel
    stopManager wm

performCommand :: MVar (Async ()) -> [Command] -> Event -> IO ()
performCommand mvar cmds0 event = do
    case makeCmds cmds0 of
        [] -> pure ()
        cmds -> do
            putStrLn ""
            putStrLnColor Cyan (showEvent event)

            modifyMVar_ mvar $ \a -> do
                cancel a
                async (runCommands cmds)
  where
    -- If no commands were provided, infer the command based on filename.
    makeCmds :: [Command] -> [Command]
    makeCmds [] =
        case takeExtension (eventPath event) of
            ".hs"  -> ["stack ghc " ++ eventPath event]
            ".erl" -> ["rebar compile"]
            _      -> []
    makeCmds cs = cs

runCommands :: [Command] -> IO ()
runCommands [] = pure ()
runCommands (cmd:cmds) = do
    putStrColor Magenta "\n> "
    putStrLn cmd

    bracketOnError (runCommand cmd) terminateProcess $ \ph -> do
        exitCode <- waitForProcess ph
        case exitCode of
            ExitSuccess -> do
                putStrLnColor Green "Success ✓"
                runCommands cmds
            _ ->
                putStrLnColor Red "Failure ✗"

putStrColor :: Color -> String -> IO ()
putStrColor c s = do
    setSGR [SetColor Foreground Dull c]
    putStr s
    setSGR [Reset]

putStrLnColor :: Color -> String -> IO ()
putStrLnColor c s = do
    setSGR [SetColor Foreground Dull c]
    putStrLn s
    setSGR [Reset]

showEvent :: Event -> String
showEvent (Added fp _)    = "Added " ++ show fp
showEvent (Modified fp _) = "Modified " ++ show fp
showEvent (Removed fp _)  = "Removed " ++ show fp
