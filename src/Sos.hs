module Sos
  ( sosEnqueueJobs
  , module Sos.Exception
  , module Sos.FileEvent
  , module Sos.Job
  , module Sos.JobQueue
  , module Sos.Rule
  , module Sos.Template
  ) where

import Sos.Exception
import Sos.FileEvent
import Sos.Job
import Sos.JobQueue
import Sos.Rule
import Sos.Template

import Control.Applicative
import Control.Monad.Catch (MonadThrow, throwM)
import Data.List.NonEmpty (NonEmpty(..))
import Streaming
import Text.Regex.TDFA (match)

import qualified Streaming.Prelude as S


-- | Enqueue jobs on the given job queue resulting from to apply the given rules
-- to each emitted file event from the given stream. This function returns when
-- the stream is exhausted (which should be never).
sosEnqueueJobs
  :: (MonadIO m, MonadThrow m)
  => [Rule] -> Stream (Of FileEvent) m a -> JobQueue -> m a
sosEnqueueJobs rules events queue =
  S.mapM_
    (\(event, cmds) -> liftIO (enqueueJob event cmds queue))
    (jobStream rules events)

-- | Given a list of rules to apply to each file event, and a stream of such
-- events, re-emit the file events paired with its shell commands to run. File
-- events that trigger no shell commands are ignored (hence the NonEmpty).
jobStream
  :: forall m a.
     MonadThrow m
  => [Rule]
  -> Stream (Of FileEvent) m a
  -> Stream (Of (FileEvent, NonEmpty ShellCommand)) m a
jobStream rules events =
  S.for events
    (\event ->
      lift (commands event) >>= \case
        []     -> pure ()
        (c:cs) -> S.yield (event, c :| cs))
 where
  commands :: FileEvent -> m [ShellCommand]
  commands event = concat <$> mapM go rules
   where
    go :: Rule -> m [ShellCommand]
    go rule =
      case match (ruleRegex rule) (fileEventPath event) of
        []     -> pure []
        (xs:_) ->
          mapM
            (either throwM pure . instantiateTemplate xs)
            (ruleTemplates rule)
