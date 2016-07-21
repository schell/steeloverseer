module ANSI where

import Data.Monoid
import System.Console.ANSI

colored :: Color -> String -> String
colored c s = color c <> s <> reset

color :: Color -> String
color c = setSGRCode [SetColor Foreground Dull c]

reset :: String
reset = setSGRCode [Reset]

cyan, green, magenta, red, yellow :: String -> String
cyan    = colored Cyan
green   = colored Green
magenta = colored Magenta
red     = colored Red
yellow  = colored Yellow
