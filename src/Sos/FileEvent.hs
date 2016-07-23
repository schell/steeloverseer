{-# LANGUAGE LambdaCase #-}

module Sos.FileEvent
  ( FileEvent(..)
  , fileEventPath
  ) where

import Data.ByteString (ByteString)

data FileEvent
  = FileAdded    ByteString
  | FileModified ByteString

fileEventPath :: FileEvent -> ByteString
fileEventPath = \case
  FileAdded    path -> path
  FileModified path -> path
