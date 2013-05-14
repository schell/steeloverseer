{-#LANGUAGE RecordWildCards #-}
module Main where

import System.Environment       (getArgs )
import System.Console.GetOpt 
import Data.List
import Control.Monad

import Prelude hiding (FilePath)

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
                       , optExtensions  :: [String]
                       } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optCommands = []
                         , optExtensions = []
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option "v" ["version"]
              (NoArg (\opts -> opts { optShowVersion = True }))
              "show version info"
          , Option "c" ["command"]
              (ReqArg (\c opts -> opts { optCommands = optCommands opts ++ [c] }) "COMMAND")
              "add command to run on file event"
          , Option "e" ["extension"]
              (ReqArg (\e opts -> opts { optExtensions = optExtensions opts ++ [e] }) "EXTENSION")
              "add file extension to watch for events"
          ]
                         
header :: String
header = "Usage: sos [v] -c command -e file_extension"

version :: String
version = intercalate "\n" [ "\nSteel Overseer 0.1.0.2"
                           , "    by Schell Scivally" 
                           , ""
                           ]

startWithOpts :: Options -> IO ()
startWithOpts opts = do
    let Options{..} = opts
        haveOptions = not (null optCommands) && not (null optExtensions)
    when optShowVersion $ putStrLn version
    when haveOptions $ steelOverseer optCommands optExtensions 
     
