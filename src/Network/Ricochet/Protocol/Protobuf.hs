{-# LANGUAGE RankNTypes #-}
module Network.Ricochet.Protocol.Protobuf
  ( ext
  , msg
  ) where

import           Control.Lens
import           Control.Monad
import           Data.ByteString          (ByteString ())
import           Text.ProtocolBuffers

ext :: (MonadPlus c, ExtKey c) => Key c msg v -> Traversal' msg (c v)
ext k = lens ((^? _Right) . getExt k) (flip $ putExt k . maybe mzero id) . _Just

msg :: (ReflectDescriptor msg, Wire msg) => Prism' ByteString msg
msg = lazy . prism' messagePut ((^? _Right . _1) . messageGet)
