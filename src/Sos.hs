module Sos
  ( Sos
  , runSos
  , sosEnqueueJobs
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
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Data.List.NonEmpty           (NonEmpty(..))
import Streaming
import System.Exit
import Text.Regex.TDFA              (match)

import qualified Streaming.Prelude as S


type Sos a = ResourceT (ExceptT SosException IO) a

-- | Run an 'Sos' action in IO, exiting if any 'SosException's are thrown.
runSos :: Sos a -> IO a
runSos act =
  runExceptT (runResourceT act) >>= \case
    Left err -> do
      print err
      exitFailure
    Right x -> return x

-- | Enqueue jobs on the given job queue resulting from to apply the given
-- rules to each emitted file event from the given stream. This function
-- returns when the stream is exhausted.
sosEnqueueJobs
  :: (Applicative m, MonadError SosException m, MonadResource m)
  => [Rule]
  -> Stream (Of FileEvent) m a
  -> JobQueue
  -> m a
sosEnqueueJobs rules events queue =
  S.mapM_
    (\(event, cmds) -> liftIO (enqueueJob event cmds queue))
    (jobStream rules events)

jobStream
  :: forall m a.
     (Applicative m, MonadError SosException m)
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
        (xs:_) -> mapM (instantiateTemplate xs) (ruleTemplates rule)
