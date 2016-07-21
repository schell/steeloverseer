{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Sos.Job
  ( Job(jobCommands)
  , JobQueue
  , JobResult(..)
  , ShellCommand
  , newJobQueue
  , clearJobQueue
  , jobQueueLength
  , jobQueueJobs
  , enqueueJob
  , stepJobQueue
  ) where

import Sos.Utils

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Foldable            (find)
import Data.List.NonEmpty       (NonEmpty(..))
import Data.Monoid
import Data.Sequence            (Seq, ViewL(..), (|>), viewl)
import System.Exit
import System.Process
import Text.Printf

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence      as Sequence


type ShellCommand = String


data JobResult = JobSuccess | JobFailure


-- | A 'Job' is an interruptible list of shell commands to run.
data Job = Job
  { jobHeader   :: String
    -- ^ String to print before running the job
  , jobCommands :: NonEmpty ShellCommand
    -- ^ The list of shell commands to run.
  , jobRestart  :: TMVar ()
    -- ^ A TMVar that, when written to, indicates this job should be
    -- immediately canceled and restarted.
  }

makeJob :: String -> NonEmpty ShellCommand -> STM Job
makeJob header cmds = do
  tmvar <- newEmptyTMVar
  pure (Job header cmds tmvar)

-- | A 'JobQueue' is a mutable linked list of jobs. The job at the head of the
-- list is assumed to be currently running, however, it is left in the list so
-- it can be interrupted and restarted.
newtype JobQueue = JobQueue (TVar (Seq Job))

newJobQueue :: IO JobQueue
newJobQueue = JobQueue <$> newTVarIO mempty

clearJobQueue :: JobQueue -> IO ()
clearJobQueue (JobQueue queue) = atomically (writeTVar queue mempty)

jobQueueLength :: JobQueue -> IO Int
jobQueueLength queue = Sequence.length <$> jobQueueJobs queue

jobQueueJobs :: JobQueue -> IO (Seq Job)
jobQueueJobs (JobQueue queue_tv) = atomically (readTVar queue_tv)

enqueueJob :: String -> NonEmpty ShellCommand -> JobQueue -> IO ()
enqueueJob header cmds (JobQueue queue_tv) = atomically $ do
  queue <- readTVar queue_tv

  case find (\j -> jobCommands j == cmds) queue of
    -- If the job is not already enqueued, enqueue it.
    Nothing -> do
      job <- makeJob header cmds
      writeTVar queue_tv (queue |> job)
    -- Otherwise, if it's already been enqueued, put into its reset var.
    -- This should only have an effect on the currently running job.
    Just job -> restartJob job

-- | Run the first job in the job queue, restarting it if signaled to do so.
-- When this function returns, the job is popped off the job queue, and its
-- result is returned.
stepJobQueue :: JobQueue -> IO JobResult
stepJobQueue (JobQueue queue_tv) = do
  -- Read the first job, but don't actually pop it off of the queue
  job <- atomically $ do
    queue <- readTVar queue_tv
    case viewl queue of
      j :< _ -> do
        unrestartJob j
        pure j
      _ -> retry

  putStrLn ("\n" <> cyan (jobHeader job))

  a <- async (runShellCommands (NonEmpty.toList (jobCommands job)))

  let runJob :: STM JobResult
      runJob = do
        r <- waitSTM a
        -- seqTail is safe here because this is the only function that pops
        -- jobs off of the job queue, and we already saw one job in the queue
        -- above.
        modifyTVar' queue_tv seqTail
        pure r

  atomically (runJob <|||> shouldRestartJob job) >>= \case
    -- Commands finished either successfully or unsuccessfully.
    Left result -> pure result

    -- Another filesystem event triggered this list of commands
    -- to be run again - cancel the previous run and start over.
    Right () -> do
      cancel a
      _ <- waitCatch a
      stepJobQueue (JobQueue queue_tv)

restartJob :: Job -> STM ()
restartJob job = void (tryPutTMVar (jobRestart job) ())

-- | Clear any previous restart "ping"s that were sent to this job while it was
-- sitting in the job queue (not at the front).
unrestartJob :: Job -> STM ()
unrestartJob = void . tryTakeTMVar . jobRestart

shouldRestartJob :: Job -> STM ()
shouldRestartJob = readTMVar . jobRestart

-- | Run a list of shell commands sequentially. If a command returns
-- ExitFailure, or an exception is thrown, don't run the rest (but also don't
-- propagate the exception). Return whether or not all commands completed
-- successfully.
runShellCommands :: [ShellCommand] -> IO JobResult
runShellCommands cmds0 = go 1 cmds0
 where
  go :: Int -> [ShellCommand] -> IO JobResult
  go _ [] = pure JobSuccess
  go n (cmd:cmds) = do
    putStrLn (magenta (printf "[%d/%d] " n (length cmds0)) <> cmd)

    let acquire :: IO ProcessHandle
        acquire = do
          (_, _, _, ph) <- createProcess (shell cmd)
          pure ph

    try (bracket acquire terminateProcess waitForProcess) >>= \case
      Left (ex :: SomeException) -> do
          -- We expect to get ThreadKilled exceptions when we get canceled and
          -- restarted. Any other exception would be bizarre; just print it and
          -- move on.
          case fromException ex of
            Just ThreadKilled ->
              case length cmds0 of
                -- If this was a one-command job, just print that it's been
                -- canceled. Otherwise, print a little graphic showing how much
                -- of the job was completed before being restarted
                1 -> putStrLn (yellow ("Restarting job: " ++ cmd))
                _ -> do
                  let (xs, ys) = splitAt (n-1) cmds0
                  putStrLn (yellow "Restarting job:")
                  mapM_ (putStrLn . yellow . printf "[✓] %s") xs
                  mapM_ (putStrLn . yellow . printf "[ ] %s") ys
            _ -> putStrLn (red ("Exception: " ++ show ex))
          pure JobFailure

      Right ExitSuccess -> do
        putStrLn (green "Success ✓")
        go (n+1) cmds

      Right (ExitFailure c) -> do
        putStrLn (red (printf "Failure ✗ (%d)" c))
        pure JobFailure

--------------------------------------------------------------------------------

seqTail :: Seq a -> Seq a
seqTail s =
  case viewl s of
    _ :< xs -> xs
    _       -> error "seqTail: empty sequence"
