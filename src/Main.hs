{-#LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import SOS

version :: String
version = "Steel Overseer 1.1.0.4"

data Options = Options
    { optShowVersion :: Bool
    , optCommands    :: [Command]
    , optPatterns    :: [Pattern]
    , optDirectory   :: FilePath
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
        <$> switch
            ( long "version"
           <> short 'v'
           <> help "Show version info." )
        <*> some (strOption
            ( long "command"
           <> short 'c'
           <> help "Add command to run on file event."
           <> metavar "COMMAND" ))
        <*> some (strOption
            ( long "pattern"
           <> short 'p'
           <> help "Add pattern to match on file path."
           <> metavar "PATTERN" ))
        <*> strOption
            ( long "directory"
           <> short 'd'
           <> help "Set directory to watch for changes."
           <> value "."
           <> showDefault
           <> metavar "DIRECTORY" )

main' :: Options -> IO ()
main' Options{..} = do
    if optShowVersion
        then putStrLn version
        else steelOverseer optDirectory optCommands optPatterns
