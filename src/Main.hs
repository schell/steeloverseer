module Main where

import System.Environment       (getArgs )
import System.Console.GetOpt 
import Data.List
import qualified Filesystem.Path.CurrentOS as OS

import Filesystem.Path ( FilePath )
import Prelude hiding (FilePath)

import SOS

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (flags,      [],   []) -> startWithOpts $ sort flags 
        (    _, nonOpts,   []) -> error $ "unrecognized arguments: " ++ unwords nonOpts
        (    _,       _, msgs) -> error $ concat msgs ++ usageInfo header options

data Flag = Version | Command String | Directory FilePath deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options = [ Option "vV" ["version"] (NoArg Version) "show version number"
          , Option "cC" ["command"] (ReqArg Command "command") "command to run on change"
          , Option "dD" ["directory"] (ReqArg (Directory . OS.decodeString) "directory") "directory to watch for changes"
          ]

header :: String
header = "usage: sos [vcdVCD]"

version :: String
version = intercalate "\n" [ "\nSteel Overseer 0.0.1.0"
                           , "    by Schell Scivally" 
                           , ""
                           ]

startWithOpts :: [Flag] -> IO ()
startWithOpts (Version:xs) = do
    putStrLn version
    startWithOpts xs
startWithOpts (flag:[]) = case flag of
   (Command cmd)   -> startWithOpts [Command cmd, Directory $ OS.decodeString "."]
   (Directory dir) -> startWithOpts [Command "echo changed", Directory dir]
startWithOpts [Command cmd, Directory dir] = steelOverseer cmd (dir :: FilePath)
startWithOpts _ = startWithOpts [Command "echo changed", Directory $ OS.decodeString "."]
