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
                       } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optCommands    = []
                         , optPatterns    = []
                         , optDirectory   = fromText $ T.pack "."
                         , optIsDaemon    = False
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option "v" ["version"]
              (NoArg (\opts -> opts { optShowVersion = True }))
              "Show version info."
          , Option "c" ["command"]
              (ReqArg (\c opts -> opts { optCommands = optCommands opts ++ [c] }) "COMMAND")
              "Add command to run on file event."
          , Option "p" ["pattern"]
              (ReqArg (\e opts -> opts { optPatterns = optPatterns opts ++ [e] }) "PATTERN")
              "Add pattern to match on file path." 
          , Option "d" ["directory"]
              (ReqArg (\d opts -> opts { optDirectory = decodeString d }) "DIRECTORY")
              "Set directory to watch for changes (default is ./)."
          , Option "b" ["daemon"]
              (NoArg (\opts -> opts { optIsDaemon = True }))
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
    when optShowVersion $ putStrLn version
    unless patternsValid $ error "One or more patterns are empty."
    let runSteelOverseer = steelOverseer optDirectory optCommands optPatterns optIsDaemon
    when (haveOptions && patternsValid) $ 
        if not optIsDaemon 
          then runSteelOverseer 
          else do shouldKillPrev <- isRunning pIdFile
                  when shouldKillPrev $ do
                      putStrLn "Killing previous steeloverseer daemon..."
                      killAndWait pIdFile 
                      putStrLn "Done."
                  unless shouldKillPrev $ do
                      putStrLn "Running in the background as a daemon, logging output to sos.log."
                      runDetached (Just pIdFile) (ToFile logFile) runSteelOverseer


