module Sos.Utils where

import Control.Applicative
import Data.ByteString     (ByteString)
import System.Console.ANSI

import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

(<|||>) :: Alternative f => f a -> f b -> f (Either a b)
fa <|||> fb = Left <$> fa <|> Right <$> fb
infixl 3 <|||>

-- The proper way to make a ByteString from a String - Data.ByteString.Char8
-- will snip each Char to 8 bits.
packBS :: String -> ByteString
packBS = Text.encodeUtf8 . Text.pack

unpackBS :: ByteString -> String
unpackBS = Text.unpack . Text.decodeUtf8

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
