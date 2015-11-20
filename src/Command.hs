{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Command
    ( Command
    , CommandPlan(..)
    , RawCommandPlan(..)
    , buildCommandPlan
    , buildRawCommandPlan
    , instantiateTemplate
    ) where

import Sos

import Control.Monad.Except
import Data.Aeson.Types
import Data.ByteString            (ByteString)
import Data.Either
import Data.Monoid
import Data.Text                  (Text)
import Text.Megaparsec
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString (compile)

import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder    as BS

-- A Command is a shell command to run, e.g. "echo foo bar"
type Command = String

-- A CommandTemplate is a string that may contain anti-quoted capture groups.
-- For example,
--
--    "gcc -c {0}.c -o {0}.c"
--
-- will become
--
--    [Right "gcc -c ", Left 0, Right ".c -o ", Left 0, Right ".c"]
type CommandTemplate
    = [Either Int ByteString]

parseCommandTemplate :: MonadError SosException m => ByteString -> m CommandTemplate
parseCommandTemplate template =
    case runParser parser "" template of
        Left err -> throwError (SosCommandParseException template err)
        Right x  -> pure x
  where
    parser :: Parsec ByteString CommandTemplate
    parser = some (Right <$> textPart
          <|> Left  <$> capturePart)
      where
        textPart :: Parsec ByteString ByteString
        textPart = BS.pack <$> some (satisfy (/= '{'))

        capturePart :: Parsec ByteString Int
        capturePart = between (char '{') (char '}') (read <$> some digitChar)

-- Instantiate a template with a list of captured variables, per their indices.
--
-- For example,
--
--    instantiateTemplate ["ONE", "TWO"] [Right "foo", Left 0, Right "bar", Left 1] == "fooONEbarTWO"
--
instantiateTemplate :: forall m. MonadError SosException m => [ByteString] -> CommandTemplate -> m Command
instantiateTemplate vars0 template0 = go 0 vars0 template0
  where
    go :: Int -> [ByteString] -> CommandTemplate -> m Command
    go _ [] template =
        case flattenTemplate template of
            Left err -> throwError (SosCommandApplyException template0 vars0 err)
            Right x  -> pure x
    go n (t:ts) template = go (n+1) ts (map f template)
      where
        f :: Either Int ByteString -> Either Int ByteString
        f (Left n')
            | n == n'   = Right t
            | otherwise = Left n'
        f x = x

-- Attempt to flatten a list of Rights to a single string.
flattenTemplate :: CommandTemplate -> Either String Command
flattenTemplate = go mempty
  where
    go :: BS.Builder -> CommandTemplate -> Either String Command
    go acc [] = Right (BSL.unpack (BS.toLazyByteString acc))
    go acc (Right x : xs) = go (acc <> BS.byteString x) xs
    go _   (Left n : _) = Left ("uninstantiated template variable {" <> show n <> "}")

-- A CommandPlan is a regex paired with a list of templates to execute on files
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
data CommandPlan = CommandPlan
    { cmdPattern   :: ByteString        -- Text from which the regex was compiled.
    , cmdRegex     :: Regex             -- Compiled regex of command pattern.
    , cmdTemplates :: [CommandTemplate] -- Command template.
    }

-- Build a command plan from a "raw" command plan by compiling the regex and
-- parsing each command template.
buildCommandPlan :: forall m. MonadError SosException m => ByteString -> [ByteString] -> m CommandPlan
buildCommandPlan pattern templates0 = do
    templates <- mapM parseCommandTemplate templates0

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

    pure (CommandPlan pattern regex templates)

-- A "raw" CommandPlan that is post-processed after being parsed from a yaml
-- file. Namely, the regex is compiled and the commands are parsed into
-- templates.
data RawCommandPlan = RawCommandPlan Text [Text]

instance FromJSON RawCommandPlan where
    parseJSON (Object o) = RawCommandPlan
        <$> o .: "pattern"
        <*> o .: "commands"
    parseJSON v = typeMismatch "command" v

buildRawCommandPlan :: forall m. MonadError SosException m => RawCommandPlan -> m CommandPlan
buildRawCommandPlan (RawCommandPlan pattern templates) =
    buildCommandPlan (T.encodeUtf8 pattern) (map T.encodeUtf8 templates)
