module Main where

import System.Environment       (getArgs )
import System.Console.GetOpt 
import Data.List

import Prelude hiding (FilePath)

import SOS

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (flags, exts,   []) -> startWithOpts (sort flags) exts 
        (    _,    _, msgs) -> error $ concat msgs ++ usageInfo header options

data Flag = Version | Command String deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options = [ Option "vV" ["version"] (NoArg Version) "show version number"
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
startWithOpts [Command cmd] exts = steelOverseer cmd exts 
startWithOpts xs exts = error $ extras ++ usageInfo header options
    where extras = if null xs && null exts 
                   then "Needs at least some options."
                   else intercalate "\n    " [ "Cannot determine options:"
                                             , show xs
                                             , "With extensions:"
                                             , show exts
                                             ]
