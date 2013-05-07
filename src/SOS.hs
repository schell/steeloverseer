module SOS where

import ANSIColors

import System.FSNotify
import System.Process
import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.Maybe

import Filesystem.Path.CurrentOS as OS
import Data.Text as T
import Data.List as L

import System.IO        ( Handle )

import Prelude hiding   ( FilePath )

type RunningProcess = IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

steelOverseer :: String -> [String] -> IO ()
steelOverseer cmd exts = do
    putStrLn $ startMsg cmd exts
    wm <- startManager
    mvar <- newEmptyMVar
    let predicate = actionPredicateForExts txtExts
        txtExts   = L.map T.pack exts
        action    = performCommand (Just mvar) txtCmd
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

performCommand :: Maybe (MVar [Event]) -> Text -> Event -> IO ()
performCommand mMvar cmd event =
    case mMvar of
        Nothing -> do 
            code <- system $ T.unpack cmd  
            putStrLn $ colorString ANSICyan "Steel Overseer initialized with your command with " ++ show code ++ ".\n" 
        
        Just mvar -> do
            mEvents <- tryTakeMVar mvar
            when (isNothing mEvents) $ void $ forkFinally runCmd handler
            -- | Add the file to the list of changed files.  
            putMVar mvar $ maybe [event] (event:) mEvents 

                where handler (Right code) = do
                          print code
                          void $ tryTakeMVar mvar
                      handler (Left ex)    = putStrLn $ colorString ANSIRed "> " ++ show ex ++ "\n"

                      runCmd               = do
                          threadDelay 1000000 
                          mEvents <- tryTakeMVar mvar
                          unless (isNothing mEvents) $ mapM_ cyanPrint $ fromJust mEvents 
                          putStrLn $ colorString ANSIGreen "> " ++ T.unpack cmd
                          system $ T.unpack cmd

                      cyanPrint = putStrLn . colorString ANSICyan . show

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action andThen = mask $ \restore ->
    forkIO $ try (restore action) >>= andThen    

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
            
