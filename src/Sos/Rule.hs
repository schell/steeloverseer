module Sos.Rule
  ( Rule(..)
  , RawRule(..)
  , RawPattern
  , buildRule
  , buildRawRule
  ) where

import Sos.Exception
import Sos.Template

import Control.Applicative
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson.Compat ((.:), FromJSON(..), Parser, Value(..))
import Data.Aeson.Types (typeMismatch)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Either
import Data.Text (Text)
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString (compile)

import qualified Data.ByteString as ByteString (intercalate, singleton)
import qualified Data.Text.Encoding as Text


-- | A 'ByteString' representing a pattern, e.g. "foo\.hs" or ".*\.c"
type RawPattern = ByteString


-- A Rule is a pattern to match, optional pattern to exclude, and a list of
-- templates to execute on files that match the regex.
--
-- Any mismatching of captured variables with the associated templates will be
-- caught at runtime. For example, this definition from a .sosrc yaml file is
-- incorrect:
--
--     - pattern: .*.c
--     - commands:
--       - gcc -c \1
--
-- because there is only one capture variable, and it has with index 0.
--
data Rule = Rule
  { rulePattern   :: Regex       -- Compiled regex of file pattern.
  , ruleExclude   :: Maybe Regex -- Compiled regex of file patterns to exclude.
  , ruleTemplates :: [Template]  -- Command template.
  }

-- Build a 'Rule' from a 'RawPattern', a list of 'RawPattern' (patterns to
-- exclude), and a list of 'RawTemplate' by:
--
-- - Compiling the pattern regex
-- - Compiling the exclude regexes combined with ||
-- - Parsing each template.
--
buildRule
  :: forall m.
     MonadThrow m
  => RawPattern -> [RawPattern] -> [RawTemplate] -> m Rule
buildRule pattrn excludes templates0 = do
  templates <- mapM parseTemplate templates0

  regex <-
    -- Improve performance for patterns with no capture groups.
    case concatMap lefts templates of
      [] ->
        compileRegex
          (CompOption
            { caseSensitive  = True
            , multiline      = False
            , rightAssoc     = True
            , newSyntax      = True
            , lastStarGreedy = True
            })
          (ExecOption
            { captureGroups = False })
          pattrn
      _ -> compileRegex defaultCompOpt defaultExecOpt pattrn

  case excludes of
    [] ->
      pure (Rule
        { rulePattern = regex
        , ruleExclude = Nothing
        , ruleTemplates = templates
        })
    _ -> do
      exclude <-
        compileRegex defaultCompOpt defaultExecOpt
          (ByteString.intercalate (ByteString.singleton (c2w '|')) excludes)
      pure (Rule
        { rulePattern = regex
        , ruleExclude = Just exclude
        , ruleTemplates = templates
        })
 where
  compileRegex :: CompOption -> ExecOption -> RawPattern -> m Regex
  compileRegex co eo patt =
    case compile co eo patt of
      Left err -> throwM (SosRegexException patt err)
      Right x -> pure x

-- A "raw" Rule that is post-processed after being parsed from a yaml
-- file.
data RawRule = RawRule [RawPattern] [RawPattern] [RawTemplate]

instance FromJSON RawRule where
  parseJSON (Object o) =
    RawRule <$> parsePatterns <*> parseExcludes <*> parseCommands
   where
    parsePatterns :: Parser [RawPattern]
    parsePatterns = go ["pattern", "patterns"]

    parseExcludes :: Parser [RawPattern]
    parseExcludes = go ["exclude", "excludes", "excluding"] <|> pure []

    parseCommands :: Parser [RawTemplate]
    parseCommands = go ["command", "commands"]

    go :: [Text] -> Parser [ByteString]
    go = fmap (map Text.encodeUtf8 . listify) . asum . map (o .:)
  parseJSON v = typeMismatch "command" v

buildRawRule :: MonadThrow m => RawRule -> m [Rule]
buildRawRule (RawRule patterns excludes templates) =
  mapM (\pattrn -> buildRule pattrn excludes templates) patterns

--------------------------------------------------------------------------------

data OneOrList a
  = One a
  | List [a]
  deriving Functor

instance FromJSON a => FromJSON (OneOrList a) where
  parseJSON v@(String _) = One  <$> parseJSON v
  parseJSON v@(Array _)  = List <$> parseJSON v
  parseJSON v = typeMismatch "element or list of elements" v

listify :: OneOrList a -> [a]
listify (One x)   = [x]
listify (List xs) = xs
