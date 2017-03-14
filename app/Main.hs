{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Sos.FileEvent
import Sos.Job
import Sos.JobQueue
import Sos.Rule
import Sos.Template
import Sos.Utils

import qualified System.FSNotify.Streaming as FSNotify

import Control.Concurrent.Async
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Managed
import Data.ByteString (ByteString)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Options.Applicative
import Streaming
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import Text.Regex.TDFA (match)

import qualified Data.Foldable     as Foldable
import qualified Streaming.Prelude as S


version :: String
version = "Steel Overseer 2.0.1.0"

data Options = Options
  { optTarget   :: FilePath
  , optRCFile   :: FilePath
  , optCommands :: [RawTemplate]
  , optPatterns :: [RawPattern]
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
     ( help "Optional file or directory to watch for changes."
       <> metavar "TARGET"
       <> value "."
       <> showDefault )
    <*> strOption
     ( help "Optional rcfile to read patterns and commands from."
       <> long "rcfile"
       <> value ".sosrc"
       <> showDefault )
    <*> many (fmap packBS (strOption
      ( long "command"
        <> short 'c'
        <> help "Add command to run on file event."
        <> metavar "COMMAND" )))
    <*> many (fmap packBS (strOption
      ( long "pattern"
        <> short 'p'
        <> help "Add pattern to match on file path. Only relevant if the target is a directory. (default: .*)"
        <> metavar "PATTERN" )))

main' :: Options -> IO ()
main' Options{..} = do
  -- Parse .sosrc rules.
  rc_rules <- parseSosrc optRCFile

  -- Parse cli rules, where one rule is created per pattern that executes
  -- each of @optCommands@ sequentially.
  cli_rules <- do
    let patterns :: [RawPattern]
        patterns =
          case (rc_rules, optPatterns) of
            -- If there are no commands in .sosrc, and no patterns
            -- specified on the command line, default to ".*"
            ([], []) -> [".*"]
            _ -> optPatterns
    mapM (`buildRule` optCommands) patterns

  (target, rules) <- do
    is_dir  <- doesDirectoryExist optTarget
    is_file <- doesFileExist optTarget
    case (is_dir, is_file) of
      (True, _) -> pure (optTarget, cli_rules ++ rc_rules)
      -- If the target is a single file, completely ignore the .sosrc
      -- commands and the cli commands.
      (_, True) -> do
        rule <- buildRule (packBS optTarget) optCommands
        pure (takeDirectory optTarget, [rule])
      _ -> do
        putStrLn ("Target " ++ optTarget ++ " is not a file or directory.")
        exitFailure

  putStrLn "Hit Ctrl+C to quit."

  job_queue <- newJobQueue

  let event_stream :: Stream (Of FileEvent) Managed ()
      event_stream = watchTree target

      job_stream :: Stream (Of (FileEvent, NonEmpty ShellCommand)) Managed ()
      job_stream =
        S.for event_stream
          (\event ->
            liftIO (eventCommands rules event) >>= \case
              [] -> pure ()
              (c:cs) -> S.yield (event, c :| cs))

      enqueue_thread :: Managed ()
      enqueue_thread =
        S.mapM_
          (\(event, cmds) -> liftIO (enqueueJob event cmds job_queue))
          job_stream

      -- Run jobs forever, only stopping to prompt whether or not to run
      -- enqueued jobs when a job fails. This way, the failing job's output
      -- will not be lost by subsequent jobs' outputs without the user's
      -- consent.
      dequeue_thread :: IO a
      dequeue_thread = forever $ do
        dequeueJob job_queue >>= \case
          JobSuccess -> pure ()
          JobFailure -> do
            jobQueueLength job_queue >>= \case
              0 -> pure ()
              n -> do
                putStrLn (yellow (show n ++ " job(s) still pending."))

                hSetBuffering stdin NoBuffering
                continue <- fix (\prompt -> do
                  putStr (yellow "Press 'c' to continue, 'p' to print, or 'a' to abort: ")
                  hFlush stdout
                  (getChar <* putStrLn "") >>= \case
                    'c' -> pure True
                    'a' -> pure False
                    'p' -> do
                      jobs <- jobQueueJobs job_queue
                      Foldable.forM_ (jobCommands <$> jobs) $
                        (\(c:|cs) -> do
                          putStrLn ("- " ++ c)
                          mapM_ (putStrLn . ("  " ++)) cs)
                      prompt
                    _ -> prompt)
                hSetBuffering stdin LineBuffering

                unless continue $ do
                  n' <- jobQueueLength job_queue
                  clearJobQueue job_queue
                  putStrLn (red ("Aborted " ++ show n' ++ " job(s)."))

  race_ (runManaged enqueue_thread) dequeue_thread

eventCommands :: [Rule] -> FileEvent -> IO [ShellCommand]
eventCommands rules event = concat <$> mapM go rules
 where
  go :: Rule -> IO [ShellCommand]
  go rule =
    case match (ruleRegex rule) (fileEventPath event) of
      []     -> pure []
      (xs:_) -> mapM (instantiateTemplate xs) (ruleTemplates rule)

watchTree :: forall a. FilePath -> Stream (Of FileEvent) Managed a
watchTree target = do
  cwd <- liftIO getCurrentDirectory

  let config :: FSNotify.WatchConfig
      config = FSNotify.defaultConfig
        { FSNotify.confDebounce = FSNotify.Debounce 0.1 }

      stream :: Stream (Of FSNotify.Event) Managed a
      stream = FSNotify.watchTree config target (const True)

  S.for stream (\case
    FSNotify.Added    path _ -> S.yield (FileAdded    (go cwd path))
    FSNotify.Modified path _ -> S.yield (FileModified (go cwd path))
    FSNotify.Removed  _    _ -> pure ())
 where
  go :: FilePath -> FilePath -> ByteString
  go cwd path = packBS (makeRelative cwd path)

--------------------------------------------------------------------------------

-- Parse a list of rules from an rcfile.
parseSosrc :: FilePath -> IO [Rule]
parseSosrc sosrc = do
  exists <- doesFileExist sosrc
  if exists
    then
      decodeFileEither sosrc >>= \case
        Left err -> do
          putStrLn ("Error parsing " ++ show sosrc ++ ":\n" ++
            prettyPrintParseException err)
          exitFailure
        Right (raw_rules :: [RawRule]) -> do
          rules <- mapM buildRawRule raw_rules
          putStrLn (case length raw_rules of
                      1 -> "Found 1 rule in " ++ show sosrc
                      n -> "Found " ++ show n ++ " rules in " ++ show sosrc)
          pure (concat rules)
    else pure []

--------------------------------------------------------------------------------
-- Orphan instances

instance MonadThrow Managed where
  throwM :: Exception e => e -> Managed a
  throwM e = managed (\_ -> throwM e)
