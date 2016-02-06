{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rule
    ( Rule(..)
    , RawRule(..)
    , buildRule
    , buildRawRule
    ) where

import Sos
import Template

import Control.Applicative
import Control.Monad.Except
import Data.Aeson.Types
import Data.ByteString            (ByteString)
import Data.Either
import Data.Text                  (Text)
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString (compile)

import qualified Data.Text.Encoding as T


-- A Rule is a regex paired with a list of templates to execute on files
-- that match the regex. Any mismatching of captured variables with the
-- associated templates will be caught at runtime.
--
-- For example, this definition from a .sosrc yaml file is incorrect:
--
--     - pattern: .*.c
--     - commands:
--       - gcc -c {1}
--
-- because there is only one capture variable, and it has with index 0.
--
data Rule = Rule
    { rulePattern   :: ByteString -- Text from which the regex was compiled.
    , ruleRegex     :: Regex      -- Compiled regex of command pattern.
    , ruleTemplates :: [Template] -- Command template.
    }

-- Build a Rule from a RawRule by compiling the regex and parsing each Template.
buildRule :: forall m. MonadError SosException m => ByteString -> [ByteString] -> m Rule
buildRule pattern templates0 = do
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
        case compile comp_opt exec_opt pattern of
            Left err -> throwError (SosRegexException pattern err)
            Right x  -> pure x

    pure (Rule pattern regex templates)

-- A "raw" Rule that is post-processed after being parsed from a yaml
-- file. Namely, the regex is compiled and the commands are parsed into
-- templates.
data RawRule
    = RawRule
        [ByteString] -- patterns
        [ByteString] -- commands

instance FromJSON RawRule where
    parseJSON (Object o) = RawRule <$> parsePatterns <*> parseCommands
      where
        parsePatterns = fmap go (o .: "pattern" <|> o .: "patterns")
        parseCommands = fmap go (o .: "command" <|> o .: "commands")

        go :: OneOrList Text -> [ByteString]
        go = map T.encodeUtf8 . listify
    parseJSON v = typeMismatch "command" v

buildRawRule :: forall m. MonadError SosException m => RawRule -> m [Rule]
buildRawRule (RawRule patterns templates) =
    mapM (\pattern -> buildRule pattern templates) patterns

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

