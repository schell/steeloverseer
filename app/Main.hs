{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Sos.FileEvent
import Sos.Job
import Sos.Rule
import Sos.Template
import Sos.Utils

import Control.Concurrent.Async
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue.Extra
import Control.Exception
import Control.Monad
import Control.Monad.Managed
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Options.Applicative
import Streaming
import System.Directory
import System.Exit
import System.FilePath
import Text.Printf (printf)
import Text.Regex.TDFA (match)

import qualified Streaming.Prelude as S
import qualified System.FSNotify.Streaming as FSNotify

version :: String
version = "Steel Overseer 2.0.2"

data Options = Options
  { optTarget   :: FilePath
  , optRCFile   :: FilePath
  , optCommands :: [RawTemplate]
  , optPatterns :: [RawPattern]
  , optExcludes :: [RawPattern]
  } deriving Show

-- The concurrent sources of input to the main worker thread.
data Input
  = JobToEnqueue Job
  | JobToRun Job
  | JobResult Job (Maybe SomeException)

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
    <*> many (fmap packBS (strOption
      ( long "exclude"
        <> short 'e'
        <> help "Add pattern to exclude matches on file path. Only relevant if the target is a directory."
        <> metavar "PATTERN" )))

main' :: Options -> IO ()
main' Options{..} = do
  -- Parse .sosrc rules.
  rc_rules <- parseSosrc optRCFile

  -- Parse cli rules, where one rule is created per pattern that executes
  -- each of @optCommands@ sequentially.
  cli_rules <- do
    let patterns, excludes :: [RawPattern]
        (patterns, excludes) =
          case (rc_rules, optPatterns) of
            -- If there are no commands in .sosrc, and no patterns
            -- specified on the command line, default to ".*"
            ([], []) -> ([".*"], [])
            _ -> (optPatterns, optExcludes)

    mapM (\pattrn -> buildRule pattrn excludes optCommands) patterns

  (target, rules) <- do
    is_dir  <- doesDirectoryExist optTarget
    is_file <- doesFileExist optTarget
    case (is_dir, is_file) of
      (True, _) -> pure (optTarget, cli_rules ++ rc_rules)
      -- If the target is a single file, completely ignore the .sosrc
      -- commands.
      (_, True) -> do
        rule <- buildRule (packBS optTarget) [] optCommands
        pure (takeDirectory optTarget, [rule])
      _ -> do
        putStrLn ("Target " ++ optTarget ++ " is not a file or directory.")
        exitFailure

  putStrLn "Hit Ctrl+C to quit."

  all_job_events :: TQueue Job <- newTQueueIO

  let event_stream :: Stream (Of FileEvent) Managed ()
      event_stream = watchTree target

      job_stream :: Stream (Of Job) Managed ()
      job_stream =
        S.for event_stream
          (\event ->
            liftIO (eventCommands rules event) >>= \case
              [] -> pure ()
              (c:cs) -> S.yield (Job event (c :| cs)))

      enqueue_thread :: Managed ()
      enqueue_thread =
        S.mapM_ (liftIO . atomically . writeTQueue all_job_events) job_stream

      -- Run jobs forever, only stopping to prompt whether or not to run
      -- enqueued jobs when a job fails. This way, the failing job's output
      -- will not be lost by subsequent jobs' outputs without the user's
      -- consent.
      dequeue_thread :: IO a
      dequeue_thread = do
        -- Keep track of the subset of all job events to actually run. We don't
        -- want to enqueue already-enqueued jobs, for example - once is enough.
        jobs_to_run :: TQueue Job <- newTQueueIO

        -- The second concurrent source of input: the result of the currently-
        -- running job, if any.
        running_job :: TMVar (Job, Async ()) <- newEmptyTMVarIO

        let runJobAsync :: Job -> IO ()
            runJobAsync job = do
              putStrLn ("\n" <> cyan (showFileEvent (jobEvent job)))
              a <- async (runJob job)
              atomically (putTMVar running_job (job, a))

        forever $ do
          let input1 :: STM Input
              input1 = JobToEnqueue <$> readTQueue all_job_events

              input2 :: STM Input
              input2 = do
                (job, a) <- takeTMVar running_job
                result <- waitCatchSTM a
                pure (JobResult job (either Just (const Nothing) result))

              input3 :: STM Input
              input3 =
                isEmptyTMVar running_job >>= \case
                  True -> JobToRun <$> readTQueue jobs_to_run
                  False -> retry

          atomically (input1 <|> input2 <|> input3) >>= \case
            -- A job event occurred. If it's equal to the running job, cancel it
            -- (it will be restarted when we process its ThreadKilled result).
            -- Otherwise, enqueue it if it doesn't already exist.
            JobToEnqueue job ->
              atomically (tryReadTMVar running_job) >>= \case
                Just (job', a) | job == job' -> do
                  -- Slightly hacky here: replace the existing job with the new
                  -- one, because although they contain the same commands, when
                  -- we restart the job, we want to print the newer 'FileEvent',
                  -- which may be different.
                  _ <- atomically (swapTMVar running_job (job, a))
                  cancel a
                _ ->
                  atomically $
                    elemTQueue jobs_to_run job >>= \case
                      True -> pure ()
                      False -> writeTQueue jobs_to_run job

            JobResult job (Just ex) -> do
              case fromException ex of
                -- The currently-running job died via ThreadKilled. We will
                -- assume we 'canceled' it to restart it.
                Just ThreadKilled -> runJobAsync job

                -- The currently-running job died via some other means. We don't
                -- want the output to get lost, so we'll just empty the job
                -- queue for simplicity.
                _ -> do
                  putStrLn (prettyPrintException ex)

                  let exhaustJobsToRun :: STM ()
                      exhaustJobsToRun =
                        tryReadTQueue jobs_to_run >>= \case
                          Nothing -> pure ()
                          Just _ -> exhaustJobsToRun

                  atomically exhaustJobsToRun

            -- Job ended successfully!
            JobResult _ Nothing -> pure ()

            JobToRun job -> runJobAsync job

  race_ (runManaged enqueue_thread) dequeue_thread

eventCommands :: [Rule] -> FileEvent -> IO [ShellCommand]
eventCommands rules event = concat <$> mapM go rules
 where
  go :: Rule -> IO [ShellCommand]
  go rule =
    case (patternMatch, excludeMatch) of
      -- Pattern doesn't match
      ([], _) -> pure []
      -- Pattern matches, but so does exclude pattern
      (_, True) -> pure []
      -- Pattern matches, and exclude pattern doesn't!
      (xs, False) -> mapM (instantiateTemplate xs) (ruleTemplates rule)

   where
    patternMatch :: [ByteString]
    patternMatch =
      case match (rulePattern rule) (fileEventPath event) of
        [] -> []
        xs:_ -> xs

    excludeMatch :: Bool
    excludeMatch =
      case ruleExclude rule of
        Nothing -> False
        Just exclude -> match exclude (fileEventPath event)

watchTree :: forall a. FilePath -> Stream (Of FileEvent) Managed a
watchTree target = do
  cwd <- liftIO getCurrentDirectory

  let config :: FSNotify.WatchConfig
      config = FSNotify.defaultConfig
        { FSNotify.confDebounce = FSNotify.Debounce 0.1 }

      stream :: Stream (Of FSNotify.Event) Managed a
      stream = FSNotify.watchTree config target (const True)

  S.for stream (\case
    FSNotify.Added    path _ _ -> S.yield (FileAdded    (go cwd path))
    FSNotify.Modified path _ _ -> S.yield (FileModified (go cwd path))
    FSNotify.Removed  _    _ _ -> pure ())
 where
  go :: FilePath -> FilePath -> ByteString
  go cwd path = packBS (makeRelative cwd path)

prettyPrintException :: SomeException -> String
prettyPrintException ex =
  case fromException ex of
    Just ExitSuccess -> red (printf "Failure ✗ (0)")
    Just (ExitFailure code) -> red (printf "Failure ✗ (%d)" code)
    Nothing -> red (show ex)

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
