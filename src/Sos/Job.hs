module Sos.Job
  ( Job(..)
  , ShellCommand
  , runJob
  ) where

import Sos.FileEvent
import Sos.Utils

import Control.Applicative
import Control.Exception
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid
import System.Exit
import System.Process
import Text.Printf

import qualified Data.List.NonEmpty as NonEmpty


type ShellCommand = String

-- | A 'Job' is a list of shell commands to run, along with the 'FileEvent' that
-- triggered the job.
data Job = Job
  { jobEvent    :: FileEvent             -- ^ Event that triggered this job.
  , jobCommands :: NonEmpty ShellCommand -- ^ The list of shell commands to run.
  }

-- | Non-stanard Eq instance: Job equality compares only the shell commands it's
-- associated with.
instance Eq Job where
  (==) = (==) `on` jobCommands

-- | Run a Job's list of shell commands sequentially. If a command returns
-- ExitFailure, or an exception is thrown, propagate the exception.
runJob :: Job -> IO ()
runJob (NonEmpty.toList . jobCommands -> cmds0) = go 1 cmds0
 where
  go :: Int -> [ShellCommand] -> IO ()
  go _ [] = pure ()
  go n (cmd:cmds) = do
    putStrLn (magenta (printf "[%d/%d] " n (length cmds0)) <> cmd)

    let acquire :: IO ProcessHandle
        acquire = do
          (_, _, _, ph) <- createProcess (shell cmd)
          pure ph

    try (bracket acquire terminateProcess waitForProcess) >>= \case
      Left (ex :: SomeException) -> do
        case fromException ex of
          Just ThreadKilled -> do
            putStrLn (yellow "Job interrupted ✗")
            throwIO ThreadKilled
          _ -> do
            putStrLn (red (show ex))
            throwIO ex

      Right ExitSuccess -> do
        putStrLn (green "Success ✓")
        go (n+1) cmds

      Right (ExitFailure c) ->
        throwIO (ExitFailure c)
