module SOS where

import  System.FSNotify
import  System.Process
import  System.Exit
import  System.FilePath
import  System.Console.ANSI
import  Control.Monad
import  Control.Concurrent
import  Data.Maybe
import  Text.Regex.TDFA
import  Prelude hiding   ( FilePath )

-- | A structure to hold our changed file events,
-- list of commands to run and possibly the currently running process,
-- in case it's one that hasn't terminated.
data SOSState = SOSIdle
              | SOSPending { accumulatedEvents :: [Event] }
              | SOSRunning { runningProccess   :: ProcessHandle
                           , pendingCommands   :: [String]
                           }

-- | Starts sos in a `dir` watching `ptns` to execute `cmds`.
steelOverseer :: FilePath -> [String] -> [String] -> IO ()
steelOverseer dir cmds ptns = do
    putStrLn "Hit enter to quit.\n"
    wm <- startManager
    mvar <- newEmptyMVar

    let predicate = actionPredicateForRegexes ptns
        action    = performCommand mvar cmds
    _ <- watchTree wm dir predicate action
    _ <- getLine
    cleanup wm mvar

-- | Cleans up sos on close.
cleanup :: WatchManager -> MVar SOSState -> IO ()
cleanup wm mvar = do
    putStrLn "Cleaning up."
    mState <- tryTakeMVar mvar
    case mState of
        Just (SOSRunning pid _) -> terminatePID pid
        _ -> return ()
    stopManager wm

actionPredicateForRegexes :: [String] -> Event -> Bool
actionPredicateForRegexes ptns event =
    or (fmap (eventPath event =~) ptns :: [Bool])

performCommand :: MVar SOSState -> [String] -> Event -> IO ()
performCommand mvar cmds event = do
    cyanPrint event
    mSosState <- tryTakeMVar mvar
    sosState  <- case mSosState of
        Nothing -> do
            -- This is the first file to change and there is no running process.
            startWriteProcess mvar cmds 1000000
            return $ SOSPending [event]

        Just (SOSRunning pid _) -> do
            -- There is a hanging process.
            mExCode <- getProcessExitCode pid
            when (isNothing mExCode) $ terminatePID pid
            startWriteProcess mvar cmds 1000000
            return $ SOSPending [event]

        Just (SOSPending events) ->
            -- SOS is waiting the 1sec delay for events before running the process.
            return $ SOSPending $ event:events

        Just state -> return state

    putMVar mvar sosState

startWriteProcess :: MVar SOSState -> [String] -> Int -> IO ()
startWriteProcess mvar [] _ = do
    _ <- tryTakeMVar mvar
    return ()

startWriteProcess mvar (cmd:cmds) delay = void $ forkIO $ do
    threadDelay delay

    setSGR [SetColor Foreground Dull Green]
    putStr "\n> "
    setSGR [Reset]
    putStrLn cmd

    pId <- runCommand cmd
    mEvProcCurrent <- tryTakeMVar mvar
    case mEvProcCurrent of
        -- This shouldn't ever happen.
        Nothing -> putMVar mvar $ SOSRunning pId cmds

        Just _ -> void $ forkIO $ do
            putMVar mvar $ SOSRunning pId cmds
            exitCode <- waitForProcess pId
            case exitCode of
                ExitSuccess -> do
                    greenPrint exitCode
                    startWriteProcess mvar cmds 0
                _           -> redPrint exitCode

terminatePID :: ProcessHandle -> IO ()
terminatePID pid = do
    terminateProcess pid
    putStrLnColor Red "Terminated hanging process."

cyanPrint :: Show a => a -> IO ()
cyanPrint = putStrLnColor Cyan . show

redPrint :: Show a => a -> IO ()
redPrint = putStrLnColor Red . show

greenPrint :: Show a => a -> IO ()
greenPrint = putStrLnColor Green . show

putStrLnColor :: Color -> String -> IO ()
putStrLnColor c s = do
    setSGR [SetColor Foreground Dull c]
    putStrLn s
    setSGR [Reset]
