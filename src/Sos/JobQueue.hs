module Sos.JobQueue
  ( JobQueue
  , newJobQueue
  , clearJobQueue
  , jobQueueLength
  , jobQueueJobs
  , enqueueJob
  , dequeueJob
  ) where

import Sos.FileEvent
import Sos.Job
import Sos.Utils

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Foldable            (find)
import Data.List.NonEmpty       (NonEmpty(..))
import Data.Monoid
import Data.Sequence            (Seq, ViewL(..), (|>), viewl)

import qualified Data.Sequence as Sequence

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

enqueueJob :: FileEvent -> NonEmpty ShellCommand -> JobQueue -> IO ()
enqueueJob event cmds (JobQueue queue_tv) = atomically $ do
  queue <- readTVar queue_tv

  case find (\j -> jobCommands j == cmds) queue of
    -- If the job is not already enqueued, enqueue it.
    Nothing -> do
      job <- newJob event cmds
      writeTVar queue_tv (queue |> job)
    -- Otherwise, if it's already been enqueued, put into its reset var.
    -- This should only have an effect on the currently running job.
    Just job -> restartJob job

-- | Run the first job in the job queue, restarting it if signaled to do so.
-- When this function returns, the job is popped off the job queue, and its
-- result is returned.
dequeueJob :: JobQueue -> IO JobResult
dequeueJob (JobQueue queue_tv) = do
  -- Read the first job, but don't actually pop it off of the queue
  job <- atomically $ do
    queue <- readTVar queue_tv
    case viewl queue of
      j :< _ -> do
        unrestartJob j
        pure j
      _ -> retry

  putStrLn ("\n" <> cyan (showFileEvent (jobEvent job)))

  a <- async (runJob job)

  let doRunJob :: STM JobResult
      doRunJob = do
        r <- waitSTM a
        -- seqTail is safe here because this is the only function that pops
        -- jobs off of the job queue, and we already saw one job in the queue
        -- above.
        modifyTVar' queue_tv seqTail
        pure r

  atomically (doRunJob <|||> shouldRestartJob job) >>= \case
    -- Commands finished either successfully or unsuccessfully.
    Left result -> pure result

    -- Another filesystem event triggered this list of commands
    -- to be run again - cancel the previous run and start over.
    Right () -> do
      cancel a
      _ <- waitCatch a
      dequeueJob (JobQueue queue_tv)

--------------------------------------------------------------------------------

seqTail :: Seq a -> Seq a
seqTail s =
  case viewl s of
    _ :< xs -> xs
    _       -> error "seqTail: empty sequence"

showFileEvent :: FileEvent -> String
showFileEvent = \case
  FileAdded    path -> unpackBS ("Added: "    <> path)
  FileModified path -> unpackBS ("Modified: " <> path)
