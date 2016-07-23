module Sos.JobQueueSpec where

import Sos.FileEvent
import Sos.JobQueue

import Test.Hspec

import qualified Data.List.NonEmpty as NonEmpty

spec :: Spec
spec =
  describe "enqueueJob" $ do
    it "enqueues jobs" $ do
      queue <- newJobQueue
      enqueueJob (FileAdded "foo.txt") (NonEmpty.fromList ["a","b","c"]) queue
      enqueueJob (FileAdded "bar.txt") (NonEmpty.fromList ["d","e","f"]) queue
      jobQueueLength queue `shouldReturn` 2

    it "does not double-enqueue" $ do
      queue <- newJobQueue
      enqueueJob (FileAdded "foo.txt") (NonEmpty.fromList ["a","b","c"]) queue
      enqueueJob (FileAdded "bar.txt") (NonEmpty.fromList ["a","b","c"]) queue
      jobQueueLength queue `shouldReturn` 1
