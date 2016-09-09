-- | A streaming wrapper around System.FSNotify.

module System.FSNotify.Streaming
  ( System.FSNotify.Streaming.watchTree
    -- * Re-exports
  , Event(..)
  , WatchManager
  , defaultConfig
  ) where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans.Resource
import Streaming
import Streaming.Prelude (yield)
import System.FSNotify

watchTree
  :: MonadResource m
  => WatchConfig -> FilePath -> (Event -> Bool) -> Stream (Of Event) m a
watchTree config path predicate = do
  chan         <- liftIO newChan
  (_, manager) <- allocate (startManagerConf config) stopManager
  _            <- allocate (watchTreeChan manager path predicate chan) id

  forever (liftIO (readChan chan) >>= yield)
