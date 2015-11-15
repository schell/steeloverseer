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

-- | Starts sos in a `dir` watching `ptns` to execute `cmds`.
steelOverseer :: FilePath -> [Command] -> [Pattern] -> IO ()
steelOverseer dir cmds ptns = do
    putStrLn "Hit enter to quit."
    wm <- startManager

    mvar <- newMVar =<< async (pure ())

    let predicate = actionPredicateForRegexes ptns
        action    = performCommand mvar cmds
    _ <- watchTree wm dir predicate action

    _ <- getLine
    takeMVar mvar >>= cancel
    stopManager wm

actionPredicateForRegexes :: [Pattern] -> Event -> Bool
actionPredicateForRegexes ptns event =
    or (fmap (eventPath event =~) ptns :: [Bool])

performCommand :: MVar (Async ()) -> [Command] -> Event -> IO ()
performCommand mvar cmds event = do
    putStrLn ""
    putStrLnColor Cyan (showEvent event)

    modifyMVar_ mvar $ \a -> do
        cancel a
        async (runCommands cmds)

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
