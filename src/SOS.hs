module SOS where

import ANSIColors

import System.FSNotify
import System.Process
import System.Exit
import Control.Monad
import Control.Concurrent
import Data.Maybe

import Filesystem.Path.CurrentOS as OS
import Data.Text as T
import Data.List as L

import System.IO        ( Handle )

import Prelude hiding   ( FilePath )

type RunningProcess = IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

-- | A tuple to hold our changed file events, list of commands to run and possibly the currently running process, in case it's one that hasn't terminated.
type SOSState = ([Event], [String], Maybe ProcessHandle)

steelOverseer :: [String] -> [String] -> IO ()
steelOverseer cmds exts = do
    putStrLn $ startMsg cmds exts
    wm <- startManager
    mvar <- newEmptyMVar
    let predicate = actionPredicateForExts $ L.map T.pack exts
        action    = performCommand mvar cmds in
      watchTree wm curdir predicate action

    _ <- getLine
    stopManager wm

curdir :: OS.FilePath
curdir = fromText $ T.pack "."

actionPredicateForExts :: [Text] -> Event -> Bool 
actionPredicateForExts exts event = let maybeExt = extension $ eventPath event in
    case maybeExt of
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
            when (isNothing mExCode) $ do 
                --interruptProcessGroupOf pid
                terminateProcess pid
                redPrint "Terminated hanging process."
            startWriteProcess mvar cmds 1000000
            return ([event], cmds, Nothing)

        Just (events, leftoverCmds, Nothing) -> 
            -- SOS is waiting the 1sec delay for events before running the process. 
            return (event:events, leftoverCmds, Nothing)

        Just enp -> return enp

    putMVar mvar eventsAndProcess
    
        where 

startWriteProcess :: MVar SOSState -> [String] -> Int -> IO () 
startWriteProcess mvar [] _ = do
    _ <- tryTakeMVar mvar
    putStrLn $ colorString ANSIGreen "Done."

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
                    cyanPrint exitCode
                    startWriteProcess mvar cmds 0
                _           -> redPrint exitCode

greenPrint :: (Show a) => a -> IO ()
greenPrint = colorPrint ANSIGreen

cyanPrint :: (Show a) => a -> IO ()
cyanPrint = colorPrint ANSICyan 

redPrint :: (Show a) => a -> IO ()
redPrint = colorPrint ANSIRed 

colorPrint :: (Show a) => ANSIColor -> a -> IO ()
colorPrint c = putStrLn . colorString c . show
    
startMsg :: [String] -> [String] -> String 
startMsg cmds exts = L.foldl (++) "" [ "Starting Steel Overseer to perform " 
                                     , L.intercalate ", " $  quote cmds
                                     , " when files of type "
                                     , L.intercalate ", " $ quote exts
                                     , " change in the current directory ("
                                     , currentDir 
                                     , ")\n"
                                     ]

    where quote      = L.map (\s-> "\""++s++"\"")
          currentDir = T.unpack $ case toText curdir of
                                       Left p  -> p 
                                       Right p -> p
               
            
