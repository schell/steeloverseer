{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Sos.Job
  ( Job(jobHeader, jobCommands)
  , JobResult(..)
  , ShellCommand
  , newJob
  , runJob
  , restartJob
  , unrestartJob
  , shouldRestartJob
  ) where

import Sos.Utils

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.List.NonEmpty     (NonEmpty)
import Data.Monoid
import System.Exit
import System.Process
import Text.Printf

import qualified Data.List.NonEmpty as NonEmpty


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

newJob :: String -> NonEmpty ShellCommand -> STM Job
newJob header cmds = do
  tmvar <- newEmptyTMVar
  pure (Job header cmds tmvar)

restartJob :: Job -> STM ()
restartJob job = void (tryPutTMVar (jobRestart job) ())

-- | Clear any previous restart "ping"s that were sent to this job while it was
-- sitting in the job queue (not at the front).
unrestartJob :: Job -> STM ()
unrestartJob = void . tryTakeTMVar . jobRestart

-- | An STM action that returns when this job should be restarted, and retries
-- otherwise.
shouldRestartJob :: Job -> STM ()
shouldRestartJob = readTMVar . jobRestart

-- | Run a job's list of shell commands sequentially. If a command returns
-- ExitFailure, or an exception is thrown, don't run the rest (but also don't
-- propagate the exception). Return whether or not all commands completed
-- successfully.
runJob :: Job -> IO JobResult
runJob (NonEmpty.toList . jobCommands -> cmds0) = go 1 cmds0
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
