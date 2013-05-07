module Main where

import System.Environment       (getArgs )
import System.Console.GetOpt 
import Data.List
import Data.Time.Clock
import System.FSNotify          ( Event(Modified) )

import qualified Data.Text as T

import Prelude hiding (FilePath)

import SOS

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (flags, exts,   []) -> startWithOpts (sort flags) exts 
        (    _,    _, msgs) -> error $ concat msgs ++ usageInfo header options

data Flag = Version | Initialize | Command String deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options = [ Option "vV" ["version"] (NoArg Version) "show version number"
          , Option "iI" ["init"]    (NoArg Initialize) "run command at startup"
          , Option "cC" ["command"] (ReqArg Command "command") "command to run on change"
          ]

header :: String
header = "usage: sos [vV] [iI] cC [file extensions...]"

version :: String
version = intercalate "\n" [ "\nSteel Overseer 0.0.1.0"
                           , "    by Schell Scivally" 
                           , ""
                           ]

startWithOpts :: [Flag] -> [String] -> IO ()
startWithOpts (Version:xs) exts = do
    putStrLn version
    startWithOpts xs exts
startWithOpts (Initialize:Command cmd:[]) exts = do
    utc <- getCurrentTime
    putStrLn $ intercalate "\n" [ "Initializing with your command:"
                                , "    " ++ show cmd
                                ]
    performCommand Nothing (T.pack cmd) (Modified curdir utc)
    startWithOpts [Command cmd] exts 
startWithOpts [Command cmd] exts = steelOverseer cmd exts 
startWithOpts xs exts = error $ usageInfo header options ++ extras
    where extras = intercalate "\n    " [ "Cannot determine options:"
                                        , show xs
                                        , "With extensions:"
                                        , show exts
                                        ]
