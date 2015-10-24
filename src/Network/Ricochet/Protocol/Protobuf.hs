{-# LANGUAGE RankNTypes #-}
module Network.Ricochet.Protocol.Protobuf
  ( ext
  ) where

import           Control.Lens
import           Control.Monad
import           Text.ProtocolBuffers

ext :: (MonadPlus c, ExtKey c) => Key c msg v -> Traversal' msg (c v)
ext k = lens ((^? _Right) . getExt k) (flip $ putExt k . maybe mzero id) . _Just
