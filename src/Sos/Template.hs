module Sos.Template
  ( RawTemplate
  , Template
  , parseTemplates
  , instantiateTemplate
  ) where

import Sos.Exception
import Sos.Job (ShellCommand)
import Sos.Utils

import Control.Applicative
import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString (ByteString)
import Text.ParserCombinators.ReadP

import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as LText


-- | A 'RawTemplate' represents a shell command, possibly containing capture
-- groups, e.g. "ghc \0"
type RawTemplate = ByteString

-- A 'Template' is a parsed 'RawTemplate' that replaces all capture groups with
-- Lefts.
--
-- For example, the raw template
--
--    "gcc -c \1.c -o \1.c"
--
-- will become
--
--    [Right "gcc -c ", Left 1, Right ".c -o ", Left 1, Right ".c"]
--
type Template = [Either Int ByteString]


parseTemplates :: MonadThrow m => RawTemplate -> m [Template]
parseTemplates raw_template =
  case readP_to_S parser (unpackBS raw_template) of
    [(template, "")] -> pure template
    _ -> throwM (SosCommandParseException raw_template)
 where
  parser :: ReadP [Template]
  parser = sepBy1 parserSingle (packBS <$> string "||")

  parserSingle :: ReadP Template
  parserSingle = some (capturePart <|||> textPart) <* eof
   where
    capturePart :: ReadP Int
    capturePart = read <$> (char '\\' *> munch1 digit)
     where
      digit :: Char -> Bool
      digit c = c >= '0' && c <= '9'

    textPart :: ReadP ByteString
    textPart = packBS <$> munch1 (/= '\\')

-- Instantiate a template with a list of captured variables, per their indices.
--
-- For example,
--
--    instantiateTemplate ["ONE", "TWO"] [Right "foo", Left 0, Right "bar", Left 1] == "fooONEbarTWO"
--
instantiateTemplate
  :: forall m. MonadThrow m => [ByteString] -> Template -> m ShellCommand
instantiateTemplate vars0 template0 = go 0 vars0 template0
 where
  go :: Int -> [ByteString] -> Template -> m ShellCommand
  go _ [] template =
    case flattenTemplate template of
      Left n ->
        let err = "uninstantiated template variable: \\" ++ show n
        in throwM (SosCommandApplyException template0 vars0 err)
      Right x -> pure x
  go n (t:ts) template = go (n+1) ts (map f template)
   where
    f :: Either Int ByteString -> Either Int ByteString
    f (Left n')
        | n == n'   = Right t
        | otherwise = Left n'
    f x = x

-- Attempt to flatten a list of Rights to a single string.
flattenTemplate :: Template -> Either Int ShellCommand
flattenTemplate = go mempty
 where
  go :: LText.Builder -> Template -> Either Int ShellCommand
  go !acc [] = Right (LText.unpack (LText.toLazyText acc))
  go !acc (x:xs) =
    case x of
      Right s ->
        let acc' = acc <> LText.fromText (Text.decodeUtf8 s)
        in go acc' xs
      Left n -> Left n
