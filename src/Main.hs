{-#LANGUAGE RecordWildCards #-}
module Main where

import System.Environment        (getArgs)
import System.Console.GetOpt
import Control.Monad
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
                       } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optCommands    = []
                         , optPatterns    = []
                         , optDirectory   = "."
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
              (ReqArg (\d opts -> opts { optDirectory = d }) "directory")
              "Set directory to watch for changes (default is ./)."
          ]

header :: String
header = "Usage: sos [vb] -c command -p pattern"

version :: String
version = "\nSteel Overseer 1.1.1.0\n"

startWithOpts :: Options -> IO ()
startWithOpts opts = do
    let Options{..} = opts
        haveOptions   = not (null optPatterns)
        patternsValid = and $ fmap (not . null) optPatterns
    when optShowVersion $ putStrLn version
    unless patternsValid $ error "One or more patterns are empty."
    let runSteelOverseer = steelOverseer optDirectory optCommands optPatterns
    when (haveOptions && patternsValid) runSteelOverseer
