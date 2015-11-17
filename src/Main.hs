{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Either
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
    , optCommands :: [Command]
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
    let patterns = case optPatterns of
                       [] -> [".*"]
                       _  -> optPatterns
        regexs0 = map compile' patterns

    (target, regexs, watch_fn) <- do
        is_dir  <- doesDirectoryExist optTarget
        is_file <- doesFileExist optTarget
        case (is_dir, is_file) of
            (True, _) -> pure (optTarget, regexs0, watchTree)
            (_, True) -> pure (takeDirectory optTarget, [compile' optTarget], watchDir)
            _         -> do
                putStrLn ("Target " ++ optTarget ++ " is not a file or directory.")
                exitFailure

    case lefts regexs of
        []   -> steelOverseer target optCommands (rights regexs) watch_fn
        errs -> mapM_ putStrLn errs
  where
    compile' :: String -> Either String Regex
    compile' = compile defaultCompOpt defaultExecOpt
