{-#LANGUAGE RecordWildCards #-}
module Main where

import System.Environment        (getArgs)
import System.Console.GetOpt 
import System.Posix.Daemon
import Data.List
import Control.Monad
import Filesystem.Path.CurrentOS hiding (concat, null)
import Data.Text as T            hiding (foldl, concat, intercalate, null)
import Prelude                   hiding (FilePath)
import SOS

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (   [],    _,   []) -> error $ usageInfo header options
        ( opts,    _,   []) -> startWithOpts $ foldl (flip id) defaultOptions opts
        (    _,    _, msgs) -> error $ concat msgs ++ usageInfo header options

data Options = Options { optShowVersion :: Bool
                       , optCommands    :: [String]
                       , optPatterns    :: [String]
                       , optDirectory   :: FilePath
                       , optIsDaemon    :: Bool
                       , optDaemonCmd   :: String
                       } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optCommands    = []
                         , optPatterns    = []
                         , optDirectory   = fromText $ T.pack "."
                         , optIsDaemon    = False
                         , optDaemonCmd   = "start"  
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option "v" ["version"]
              (NoArg (\opts -> opts { optShowVersion = True }))
              "Show version info."
          , Option "c" ["command"]
              (ReqArg (\c opts -> opts { optCommands = optCommands opts ++ [c] }) "command")
              "Add command to run on file event."
          , Option "p" ["pattern"]
              (ReqArg (\e opts -> opts { optPatterns = optPatterns opts ++ [e] }) "pattern")
              "Add pattern to match on file path." 
          , Option "d" ["directory"]
              (ReqArg (\d opts -> opts { optDirectory = decodeString d }) "directory")
              "Set directory to watch for changes (default is ./)."
          , Option "b" ["daemon"]
              (ReqArg (\s opts -> opts { optIsDaemon = True, optDaemonCmd = s }) "start|stop")
              "Attempt to run in the background as a daemon. When a previous daemon is running from your working directory this option will kill that process."
          ]
                         
header :: String
header = "Usage: sos [vb] -c command -p pattern"

version :: String
version = "\nSteel Overseer 1.0.0.0\n"

startWithOpts :: Options -> IO ()
startWithOpts opts = do
    let Options{..} = opts
        haveOptions   = not (null optPatterns)
        patternsValid = and $ fmap (not . null) optPatterns
        daemonValid   = optDaemonCmd == "start" || optDaemonCmd == "stop"
    when optShowVersion $ putStrLn version
    unless patternsValid $ error "One or more patterns are empty."
    unless daemonValid $ error "The argument to -b,--daemon must be one of start or stop."
    didKill <- if optIsDaemon && optDaemonCmd == "stop"
                 then do shouldKillPrev <- isRunning pIdFile
                         if shouldKillPrev
                           then do putStrLn "Killing previous steeloverseer daemon..."
                                   killAndWait pIdFile 
                                   putStrLn "Done."
                           else putStrLn "There is no previous daemon to kill."
                         return shouldKillPrev
                 else return False
    let runSteelOverseer = steelOverseer optDirectory optCommands optPatterns optIsDaemon
    when (haveOptions && patternsValid && daemonValid) $ 
        if not optIsDaemon 
          then runSteelOverseer 
          else unless didKill $ do
                   putStrLn "Running in the background as a daemon, logging output to sos.log."
                   runDetached (Just pIdFile) (ToFile logFile) runSteelOverseer


