module SOS where

import ANSIColors

import System.FSNotify
import System.Process
import System.Exit
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Text.Regex.TDFA

import Filesystem.Path.CurrentOS as OS
import Data.Text as T
import Data.List as L

import System.IO        ( Handle )

import Prelude hiding   ( FilePath )

type RunningProcess = IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

-- | A tuple to hold our changed file events, 
-- list of commands to run and possibly the currently running process, 
-- in case it's one that hasn't terminated.
type SOSState = ([Event], [String], Maybe ProcessHandle)

steelOverseer :: FilePath -> [String] -> [String] -> IO ()
steelOverseer dir cmds exts = do
    putStrLn "Hit enter to quit.\n" 
    wm <- startManager
    mvar <- newEmptyMVar
    let predicate = actionPredicateForRegexes exts
        action    = performCommand mvar cmds in
      watchTree wm dir predicate action

    _ <- getLine

    mState <- tryTakeMVar mvar

    case mState of
        Just (_, _, Just pid) -> terminatePID pid
        _ -> return ()
    
    stopManager wm

actionPredicateForRegexes :: [String] -> Event -> Bool
actionPredicateForRegexes ptns event = or (fmap (filepath =~) ptns :: [Bool])
    where filepath = case toText $ eventPath event of
                         Left f  -> T.unpack f
                         Right f -> T.unpack f

actionPredicateForExts :: [String] -> Event -> Bool 
actionPredicateForExts exts event = let maybeExt = extension $ eventPath event in
    case fmap T.unpack maybeExt of
        Just ext -> ext `elem` exts
        Nothing  -> False 

performCommand :: MVar SOSState -> [String] -> Event -> IO ()
performCommand mvar cmds event = do
    cyanPrint event
    mEvProc <- tryTakeMVar mvar
    eventsAndProcess <- case mEvProc of
        Nothing -> do 
            -- This is the first file to change and there is no running process. 
            startWriteProcess mvar cmds 1000000
            return ([event], cmds, Nothing)

        Just ([], _, Just pid) -> do
            -- There is a hanging process.
            mExCode <- getProcessExitCode pid
            when (isNothing mExCode) $ terminatePID pid
            startWriteProcess mvar cmds 1000000
            return ([event], cmds, Nothing)

        Just (events, leftoverCmds, Nothing) -> 
            -- SOS is waiting the 1sec delay for events before running the process. 
            return (event:events, leftoverCmds, Nothing)

        Just enp -> return enp

    putMVar mvar eventsAndProcess

startWriteProcess :: MVar SOSState -> [String] -> Int -> IO () 
startWriteProcess mvar [] _ = do
    _ <- tryTakeMVar mvar
    return ()

startWriteProcess mvar (cmd:cmds) delay = void $ forkIO $ do
    threadDelay delay
    putStrLn $ colorString ANSIGreen "\n> " ++ cmd ++ "\n"
    pId <- runCommand cmd
    mEvProcCurrent <- tryTakeMVar mvar
    case mEvProcCurrent of
        Nothing -> putMVar mvar ([], cmds, Just pId)
        
        Just (_, _, _) -> void $ forkIO $ do
            putMVar mvar ([], cmds, Just pId)
            exitCode <- waitForProcess pId
            case exitCode of 
                ExitSuccess -> do
                    greenPrint exitCode
                    startWriteProcess mvar cmds 0
                _           -> redPrint exitCode

terminatePID :: ProcessHandle -> IO ()
terminatePID pid = do
    terminateProcess pid 
    putStrLn $ colorString ANSIRed "Terminated hanging process."

