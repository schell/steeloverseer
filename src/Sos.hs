{-# LANGUAGE LambdaCase #-}

module Sos
  ( Sos
  , runSos
  , module Sos.Exception
  , module Sos.Job
  , module Sos.Rule
  , module Sos.Template
  ) where

import Sos.Exception
import Sos.Job
import Sos.Rule
import Sos.Template

import Control.Monad.Except
import System.Exit

type Sos a = ExceptT SosException IO a

runSos :: Sos a -> IO a
runSos act =
  runExceptT act >>= \case
    Left err -> do
      print err
      exitFailure
    Right x -> return x
