module SOS where

import ANSIColors

import System.FSNotify
import System.Process
import Control.Monad
import Control.Concurrent
import Data.Maybe

import Filesystem.Path.CurrentOS as OS
import Data.Text as T
import Data.List as L

import System.IO        ( Handle )

import Prelude hiding   ( FilePath )

type RunningProcess = IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

-- | A tuple to hold our changed file events and possibly the currently running command, in case it's one that hasn't terminated.
type EventsAndProcess = ([Event], Maybe ProcessHandle)

steelOverseer :: String -> [String] -> IO ()
steelOverseer cmd exts = do
    putStrLn $ startMsg cmd exts
    wm <- startManager
    mvar <- newEmptyMVar
    let predicate = actionPredicateForExts txtExts
        txtExts   = L.map T.pack exts
        action    = performCommand mvar txtCmd
        txtCmd    = T.pack cmd in
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

performCommand :: MVar EventsAndProcess -> Text -> Event -> IO ()
performCommand mvar cmd event = do
    mEvProc <- tryTakeMVar mvar
    eventsAndProcess <- case mEvProc of
        Nothing -> do -- This is the first file to change and there is no running process. 
            startWriteProcess mvar $ T.unpack cmd 
            return ([event], Nothing)

        Just ([], Just pid) -> do
            -- There is a hanging process.
            mExCode <- getProcessExitCode pid
            when (isNothing mExCode) $ do 
                --interruptProcessGroupOf pid
                terminateProcess pid
                redPrint "Terminated hanging process."
            startWriteProcess mvar $ T.unpack cmd
            return ([event], Nothing)

        Just (events, Nothing) -> 
            -- SOS is waiting the 1sec delay for events before running the process. 
            return (event:events, Nothing)

        Just enp -> return enp

    putMVar mvar eventsAndProcess
    
        where 

startWriteProcess :: MVar EventsAndProcess -> String -> IO () 
startWriteProcess mvar sCmd = void $ forkIO $ do
    threadDelay 1000000
    putStrLn $ colorString ANSIGreen "\n> " ++ sCmd ++ "\n"
    pId <- runCommand sCmd
    mEvProcCurrent <- tryTakeMVar mvar
    case mEvProcCurrent of
        Nothing -> putMVar mvar ([], Just pId)
        
        Just (events, _) -> do
            putStrLn "Running process after events: "
            mapM_ cyanPrint events
            putMVar mvar ([], Just pId)

cyanPrint :: (Show a) => a -> IO ()
cyanPrint = colorPrint ANSICyan 

redPrint :: (Show a) => a -> IO ()
redPrint = colorPrint ANSIRed 

colorPrint :: (Show a) => ANSIColor -> a -> IO ()
colorPrint c = putStrLn . colorString c . show
    
startMsg :: String -> [String] -> String 
startMsg cmd exts = L.foldl (++) "" [ "Starting Steel Overseer to perform \"" 
                                    , cmd
                                    , "\" when files of type "
                                    , L.intercalate ", " exts
                                    , " change in the current directory ("
                                    , currentDir 
                                    , ")\n"
                                    ]
    where currentDir = T.unpack $ case toText curdir of
                                       Left p  -> p 
                                       Right p -> p
            
