{-# LANGUAGE CPP #-}

module Sos.Job
  ( Job(..)
  , ShellCommand
  , runJob
  ) where

import Sos.FileEvent
import Sos.Utils

import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.ParallelIO.Local
import Control.Exception
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import System.Exit
import System.IO
import System.IO.Error (tryIOError)
import System.Posix.Process (getProcessGroupID, getProcessGroupIDOf)
import System.Posix.Signals
  (Handler(Ignore), Signal, installHandler, sigTERM, sigTTOU,
    signalProcessGroup)
import System.Posix.Terminal (setTerminalProcessGroupID)
import System.Posix.Types (ProcessGroupID)
import System.Process
import System.Process.Internals (ProcessHandle__(OpenHandle), phandle)
import Text.Printf

import qualified Data.List.NonEmpty as NonEmpty

type ShellCommand = String

-- | A 'Job' is a list of shell commands to run, along with the 'FileEvent' that
-- triggered the job.
data Job = Job
  { jobEvent    :: FileEvent               -- ^ Event that triggered this job.
  , jobCommands :: NonEmpty [ShellCommand] -- ^ The list of lists of shell commands to run.
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
  go :: Int -> [[ShellCommand]] -> IO ()
  go _ [] = pure ()
  go n (cmd:cmds) = do
    putStrLn (magenta (printf "[%d/%d] " n (length cmds0)) <> unwords cmd)

    let flushStdin :: IO ()
        flushStdin =
          hReady stdin >>= \case
            True -> getLine >> flushStdin
            False -> pure ()

    flushStdin

    try (withPool 10 (`parallel` (runForegroundProcess . shell <$> cmd))) >>= \case
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

#ifdef mingw32_HOST_OS

runForegroundProcess :: CreateProcess -> IO ExitCode
runForegroundProcess c =
  bracket acquire release waitForProcess
 where
  acquire :: IO ProcessHandle
  acquire = do
    (_, _, _, ph) <- createProcess c { create_group = True }
    pure ph

  release :: ProcessHandle -> IO ()
  release ph = do
    _ <- tryIOError (interruptProcessGroupOf ph)
    terminateProcess ph

#else

runForegroundProcess :: CreateProcess -> IO ExitCode
runForegroundProcess c =
  bracket acquire release (\(ph, _) -> waitForProcess ph)
 where
  -- Create a process (inheriting all file descriptors) in a new process group
  -- and give it terminal access.
  acquire :: IO (ProcessHandle, ProcessGroupID)
  acquire = do
    (_, _, _, ph) <- createProcess (c { create_group = True })
    readMVar (phandle ph) >>= \case
      OpenHandle pid -> do
        pgid <- getProcessGroupIDOf pid
        setTerminalProcessGroupID 0 pgid
        pure (ph, pgid)
      _ -> error "Sos.Job.runForegroundProcess: unexpected process handle"

  -- Terminate a process and take back control of the terminal.
  release :: (ProcessHandle, ProcessGroupID) -> IO ()
  release (_, pgid) = do
    _ <- tryIOError (signalProcessGroup sigTERM pgid)
    getProcessGroupID >>= ignoring sigTTOU . setTerminalProcessGroupID 0

  ignoring :: Signal -> IO a -> IO a
  ignoring sig act =
    bracket
      (installHandler sig Ignore Nothing)
      (\handler -> installHandler sig handler Nothing)
      (\_ -> act)

#endif
