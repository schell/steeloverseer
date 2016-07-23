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
import Control.Monad.Except
import Data.Aeson.Types
import Data.ByteString            (ByteString)
import Data.Either
import Data.Text                  (Text)
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString (compile)

import qualified Data.Text.Encoding as Text


-- | A 'ByteString' representing a pattern, e.g. "foo\.hs" or ".*\.c"
type RawPattern = ByteString


-- A Rule is a regex paired with a list of templates to execute on files
-- that match the regex. Any mismatching of captured variables with the
-- associated templates will be caught at runtime.
--
-- For example, this definition from a .sosrc yaml file is incorrect:
--
--     - pattern: .*.c
--     - commands:
--       - gcc -c \1
--
-- because there is only one capture variable, and it has with index 0.
--
data Rule = Rule
  { ruleRegex     :: Regex      -- Compiled regex of command pattern.
  , ruleTemplates :: [Template] -- Command template.
  }

-- Build a 'Rule' from a 'RawPattern' and a list of 'RawTemplate' by compiling
-- the pattern regex and parsing each template.
buildRule
  :: forall m. MonadError SosException m
  => RawPattern
  -> [RawTemplate]
  -> m Rule
buildRule pattrn templates0 = do
  templates <- mapM parseTemplate templates0

  -- Improve performance for patterns with no capture groups.
  let (comp_opt, exec_opt) =
        case concatMap lefts templates of
          [] -> ( CompOption
                    { caseSensitive  = True
                    , multiline      = False
                    , rightAssoc     = True
                    , newSyntax      = True
                    , lastStarGreedy = True
                    }
                , ExecOption
                    { captureGroups = False }
                )
          _ -> (defaultCompOpt, defaultExecOpt)

  regex <-
    case compile comp_opt exec_opt pattrn of
      Left err -> throwError (SosRegexException pattrn err)
      Right x  -> return x

  return (Rule regex templates)

-- A "raw" Rule that is post-processed after being parsed from a yaml
-- file. Namely, the regex is compiled and the commands are parsed into
-- templates.
data RawRule = RawRule [RawPattern] [RawTemplate]

instance FromJSON RawRule where
  parseJSON (Object o) = RawRule <$> parsePatterns <*> parseCommands
   where
    parsePatterns :: Parser [RawPattern]
    parsePatterns = fmap go (o .: "pattern" <|> o .: "patterns")

    parseCommands :: Parser [RawTemplate]
    parseCommands = fmap go (o .: "command" <|> o .: "commands")

    go :: OneOrList Text -> [ByteString]
    go = map Text.encodeUtf8 . listify
  parseJSON v = typeMismatch "command" v

buildRawRule :: forall m. MonadError SosException m => RawRule -> m [Rule]
buildRawRule (RawRule patterns templates) =
  mapM (\pattrn -> buildRule pattrn templates) patterns

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
