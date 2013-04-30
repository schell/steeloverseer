module SOS where

import System.FSNotify
import System.FSNotify.Devel
import System.Exit  
import System.Process
import Control.Monad            ( forever )

import Prelude hiding           ( FilePath )
import Filesystem.Path          ( FilePath )

steelOverseer :: String -> FilePath -> IO ()
steelOverseer cmd dir = do
    putStrLn $ startMsg cmd dir
    wm <- startManager
    let predicate = const True
        action    = command cmd in
      watchTree wm dir predicate action

    forever $ do
        char <- getChar
        case char of
            '\n' -> do stopManager wm
                       exitSuccess
            _  -> return ()

command :: String -> Event -> IO () 
command cmd event = do 
    print event
    code <- system cmd
    print code

startMsg :: String -> FilePath -> String 
startMsg cmd dir = "Starting Steel Overseer to perform \"" ++ cmd ++ "\" when " ++ show dir ++ " changes.\n"

treeExists :: WatchManager -> FilePath -> Action -> IO ()
treeExists manager dir = watchTree manager dir (existsEvents $ const True) 

