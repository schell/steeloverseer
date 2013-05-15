{-#LANGUAGE RecordWildCards #-}
module Main where

import System.Environment        (getArgs)
import System.Console.GetOpt 
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
                       , optPatterns  :: [String]
                       , optDirectory   :: FilePath
                       } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optCommands = []
                         , optPatterns = []
                         , optDirectory = fromText $ T.pack "."
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option "v" ["version"]
              (NoArg (\opts -> opts { optShowVersion = True }))
              "show version info"
          , Option "c" ["command"]
              (ReqArg (\c opts -> opts { optCommands = optCommands opts ++ [c] }) "COMMAND")
              "add command to run on file event"
          , Option "p" ["pattern"]
              (ReqArg (\e opts -> opts { optPatterns = optPatterns opts ++ [e] }) "PATTERN")
              "add pattern to match on file path" 
          , Option "d" ["directory"]
              (ReqArg (\d opts -> opts { optDirectory = decodeString d }) "DIRECTORY")
              "set directory to watch for changes (default is ./)"
          ]
                         
header :: String
header = "Usage: sos [v] -c command -p pattern"

version :: String
version = intercalate "\n" [ "\nSteel Overseer 0.2.0.0"
                           , "    by Schell Scivally" 
                           , ""
                           ]

startWithOpts :: Options -> IO ()
startWithOpts opts = do
    let Options{..} = opts
        haveOptions   = not (null optPatterns)
        patternsValid = and $ fmap (not . null) optPatterns
    when optShowVersion $ putStrLn version
    unless patternsValid $ error "One or more patterns are empty."
    when (haveOptions && patternsValid) $ steelOverseer optDirectory optCommands optPatterns 
     
