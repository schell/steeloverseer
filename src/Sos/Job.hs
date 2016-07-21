{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Sos.Job
  ( Job
  , JobQueue
  , ShellCommand
  , newJobQueue
  , enqueueJob
  , dequeueJob
  , runJob
  ) where

import ANSI

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Function         (fix)
import Data.List.NonEmpty    (NonEmpty(..))
import Data.Monoid
import Data.Sequence         (Seq, ViewL(..), (|>), viewl)
import Lens.Micro            hiding (to)
import System.Console.ANSI
import System.Exit
import System.IO
import System.Process
import Text.Printf

import qualified Data.List.NonEmpty as NE

type ShellCommand = String

data Job = Job
  { jobHeader   :: String
  , jobCommands :: NonEmpty ShellCommand
  , jobTMVar    :: TMVar ()
  }

makeJob :: String -> NonEmpty ShellCommand -> STM Job
makeJob header cmds = do
  tmvar <- newEmptyTMVar
  pure (Job header cmds tmvar)

jobEquals :: NonEmpty ShellCommand -> Job -> Bool
jobEquals cmds job = cmds == jobCommands job

data JobQueue
  = JobQueue
      (TVar (Maybe Job)) -- Currently running job
      (TVar (Seq Job))   -- Queue of jobs to run

newJobQueue :: IO JobQueue
newJobQueue = JobQueue <$> newTVarIO Nothing <*> newTVarIO mempty

enqueueJob :: String -> NonEmpty ShellCommand -> JobQueue -> IO ()
enqueueJob header cmds (JobQueue running_tv queue_tv) = atomically $
  readTVar running_tv >>= \case
    Just job | cmds `jobEquals` job ->
      restartJob job
    _ -> do
      queue <- readTVar queue_tv
      unless (seqContains jobEquals cmds queue) $ do
        new_job <- makeJob header cmds
        modifyTVar' queue_tv (|> new_job)

-- | Dequeue a Job from a JobQueue. This assumes that there is no currently
-- running job, but does not actually check.
dequeueJob :: JobQueue -> IO Job
dequeueJob (JobQueue running_tv queue_tv) = atomically $ do
  queue <- readTVar queue_tv
  case viewl queue of
    EmptyL -> retry
    (job :< jobs) -> do
      writeTVar running_tv (Just job)
      writeTVar queue_tv jobs
      pure job

runJob :: JobQueue -> Job -> IO ()
runJob job_queue@(JobQueue running_tv queue_tv)
       job@(Job header cmds tmvar) = do
  putStrLn ("\n" <> colored Cyan header)

  a <- async (runShellCommands (NE.toList cmds))

  let lhs :: STM (Either SomeException JobResult)
      lhs = waitCatchSTM a <* writeTVar running_tv Nothing

      rhs :: STM ()
      rhs = takeTMVar tmvar

  atomically (lhs <|||> rhs) >>= \case
    -- Job threw an exception - a ThreadKilled we are
    -- okay with, but anything else we should print to the
    -- console.
    Left (Left ex) ->
      case fromException ex of
        Just ThreadKilled -> pure ()
        _ -> putStrLn (colored Red ("Exception: " ++ show ex))

    -- Commands finished successfully.
    Left (Right JobSuccess) -> pure ()

    -- A command failed - if there is another enqueued list
    -- of commands to run, prompt the user to continue or
    -- abort.
    Left (Right JobFailure) -> do
      queue <- atomically (readTVar queue_tv)
      case length queue of
        0 -> pure ()
        n -> do
          putStrLn (colored Yellow (show n ++ " job(s) still pending."))

          hSetBuffering stdin NoBuffering
          continue <- fix (\prompt -> do
            putStr (colored Yellow "Press 'c' to continue, 'p' to print, or 'a' to abort: ")
            hFlush stdout
            (getChar <* putStrLn "") >>= \case
              'c' -> pure True
              'a' -> pure False
              'p' -> do
                mapM_ (\(c:|cs) -> do
                          putStrLn ("- " ++ c)
                          mapM_ (putStrLn . ("  " ++)) cs)
                      (queue^..traversed.to jobCommands)
                prompt
              _ -> prompt)
          hSetBuffering stdin LineBuffering

          -- Here, it's possible we've reported that
          -- `n` jobs were enqueued, but by the time
          -- the user decides to abort them, more
          -- were added. Oh well, delete them all.
          unless continue $ do
            n' <- atomically (length <$> swapTVar queue_tv mempty)
            putStrLn (colored Red ("Aborted " ++ show n' ++ " job(s)."))

    -- Another filesystem event triggered this list of commands
    -- to be run again - cancel the previous run and start over.
    Right () -> do
      cancel a
      _ <- waitCatch a
      runJob job_queue job

restartJob :: Job -> STM ()
restartJob job = void (tryPutTMVar (jobTMVar job) ())

data JobResult = JobSuccess | JobFailure

-- Run a list of shell commands sequentially. If a command returns ExitFailure,
-- don't run the rest. Return whether or not all commands completed
-- successfully.
runShellCommands :: [ShellCommand] -> IO JobResult
runShellCommands cmds0 = go 1 cmds0
 where
  go :: Int -> [ShellCommand] -> IO JobResult
  go _ [] = pure JobSuccess
  go n (cmd:cmds) = do
    putStrLn (colored Magenta (printf "[%d/%d] " n (length cmds0)) <> cmd)

    let create = (\(_, _, _, ph) -> ph) <$> createProcess (shell cmd)
        teardown ph = do
          putStrLn (colored Yellow ("Canceling: " ++ cmd))
          terminateProcess ph
    bracketOnError create teardown waitForProcess >>= \case
      ExitSuccess -> do
        putStrLn (colored Green "Success ✓")
        go (n+1) cmds
      _ -> do
        putStrLn (colored Red "Failure ✗")
        pure JobFailure


seqContains :: (a -> b -> Bool) -> a -> Seq b -> Bool
seqContains _ _ (viewl -> EmptyL) = False
seqContains p x (viewl -> y :< ys)
    | p x y     = True
    | otherwise = seqContains p x ys

--------------------------------------------------------------------------------

to :: (s -> a) -> (a -> Const r a) -> s -> Const r s
to f g s = Const (getConst (g (f s)))

(<|||>) :: Alternative f => f a -> f b -> f (Either a b)
fa <|||> fb = Left <$> fa <|> Right <$> fb
