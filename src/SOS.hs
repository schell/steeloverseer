{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SOS where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Monoid
import Prelude hiding (FilePath)
import System.Console.ANSI
import System.Console.Concurrent
import System.Exit
import System.FilePath
import System.FSNotify
import System.Process
import Text.Regex.TDFA

data RunningCommand = RunningCommand
    { cmdThread   :: MVar (Async ()) -- ^ Currently running command (might be done, who knows)
    , cmdPattern  :: Regex
    , cmdCommands :: [String]
    }

-- | Starts sos watching `target`, matching `ptns` to execute `cmds`.
steelOverseer
    :: FilePath
    -> [(Regex, [String])]
    -> (WatchManager -> FilePath -> ActionPredicate -> Action -> IO StopListening)
    -> IO ()
steelOverseer target cmd_infos watch_fn = withConcurrentOutput $ do
    outputConcurrentLn "Hit enter to quit."

    withManager $ \wm -> do
        runningCmds <- mapM (\(ptn, cmd) -> do
                                mv <- newMVar =<< async (pure ())
                                pure (RunningCommand mv ptn cmd))
                            cmd_infos

        let predicate = \event -> or (map (\(ptn,_) -> match ptn (eventPath event)) cmd_infos)
        _ <- watch_fn wm target predicate $ \event -> do
            outputConcurrentLn ("\n" <> colored Cyan (showEvent event))
            mapM_ (handleEvent event) runningCmds

        _ <- getLine
        mapM_ (\rc -> takeMVar (cmdThread rc) >>= cancel) runningCmds

handleEvent :: Event -> RunningCommand -> IO ()
handleEvent event RunningCommand{..} = do
    when (match cmdPattern (eventPath event)) $ do
        case makeCmds cmdCommands of
            [] -> pure ()
            cmds -> do
                a <- modifyMVar cmdThread $ \old_a -> do
                    cancel old_a
                    new_a <- async (runCommands cmds)
                    pure (new_a, new_a)
                _ <- waitCatch a
                pure ()
  where
    -- If no commands were provided, infer the command based on filename.
    makeCmds :: [String] -> [String]
    makeCmds [] =
        case takeExtension (eventPath event) of
            ".hs"  -> ["stack ghc " ++ eventPath event]
            ".erl" -> ["rebar compile"]
            _      -> []
    makeCmds cs = cs

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

colored :: Color -> String -> String
colored c s = color c <> s <> reset

color :: Color -> String
color c = setSGRCode [SetColor Foreground Dull c]

reset :: String
reset = setSGRCode [Reset]

outputConcurrentLn :: String -> IO ()
outputConcurrentLn s = outputConcurrent (s <> "\n")

showEvent :: Event -> String
showEvent (Added fp _)    = "Added " ++ show fp
showEvent (Modified fp _) = "Modified " ++ show fp
showEvent (Removed fp _)  = "Removed " ++ show fp
