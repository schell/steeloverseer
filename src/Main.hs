{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Monad
import Data.Yaml (FromJSON(..), Value(..), (.:), decodeFileEither)
import Options.Applicative
import SOS
import System.Directory
import System.Exit
import System.FilePath
import System.FSNotify
import Text.Regex.TDFA
import Text.Regex.TDFA.String (compile)

version :: String
version = "Steel Overseer 1.1.0.4"

data Options = Options
    { optTarget   :: FilePath
    , optCommands :: [String]
    , optPatterns :: [Pattern]
    } deriving Show

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> optsParser)
        ( fullDesc
       <> progDesc "A file watcher and development tool."
       <> header version )

    optsParser :: Parser Options
    optsParser = Options
        <$> strArgument
            ( help "File or directory to watch for changes."
           <> metavar "TARGET" )
        <*> many (strOption
            ( long "command"
           <> short 'c'
           <> help "Add command to run on file event. (default: inferred)"
           <> metavar "COMMAND" ))
        <*> many (strOption
            ( long "pattern"
           <> short 'p'
           <> help "Add pattern to match on file path. Only relevant if the target is a directory. (default: .*)"
           <> metavar "PATTERN" ))

main' :: Options -> IO ()
main' Options{..} = do
    -- Parse .sosrc commands, where each pattern corresponds to exactly command.
    rc_commands <- parseSosRc

    -- Parse cli commands, where one command is created per pattern that
    -- executes each of the specified sub-commands sequentially.
    cli_commands <- do
        rs <- mapM compilePattern
                  (case (rc_commands, optPatterns) of
                       -- If there are no commands in .sosrc, and no patterns
                       -- specified on the command line, default to ".*"
                       ([], []) -> [".*"]
                       _ -> optPatterns)
        pure (map (\r -> (r, optCommands)) rs)

    (target, commands, watch_fn) <- do
        is_dir  <- doesDirectoryExist optTarget
        is_file <- doesFileExist optTarget
        case (is_dir, is_file) of
            (True, _) -> pure (optTarget, cli_commands ++ rc_commands, watchTree)
            -- If the target is a single file, completely ignore the .sosrc
            -- commands and the cli commands.
            (_, True) -> do
                r <- compilePattern optTarget
                pure (takeDirectory optTarget, [(r, optCommands)], watchDir)
            _         -> do
                putStrLn ("Target " ++ optTarget ++ " is not a file or directory.")
                exitFailure

    steelOverseer target commands watch_fn

--------------------------------------------------------------------------------

type Pattern = String

data YamlCommand = YamlCommand Pattern String

instance FromJSON YamlCommand where
    parseJSON (Object o) = YamlCommand
        <$> o .: "pattern"
        <*> o .: "command"
    parseJSON _ = mzero

parseSosRc :: IO [(Regex, [String])]
parseSosRc = do
    exists <- doesFileExist ".sosrc"
    if exists
        then
            decodeFileEither ".sosrc" >>= \case
                Left err -> do
                    putStrLn ("Error parsing .sosrc: " ++ show err)
                    exitFailure
                Right cmds -> do
                    infos <- mapM (\(YamlCommand ptn cmd) -> do
                                      r <- compilePattern ptn
                                      pure (r, [cmd])) cmds
                    putStrLn ("Found " ++ show (length infos) ++ " commands in .sosrc")
                    pure infos
        else pure []

compilePattern :: Pattern -> IO Regex
compilePattern p =
    case compile defaultCompOpt defaultExecOpt p of
        Left err -> do
            putStrLn err
            exitFailure
        Right r -> pure r
