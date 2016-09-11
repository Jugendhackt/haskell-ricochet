{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Ricochet
import           Network.Ricochet.Connection
import           Network.Ricochet.Monad
import           Network.Ricochet.Types

import           Control.Concurrent          (threadDelay)
import           Control.Lens
import           Control.Monad.State
import           Data.ByteString             (ByteString (), pack)
import qualified Data.Map                    as M
import           Data.Monoid                 ((<>))
import           Network                     hiding (accept, connectTo)


config = RicochetConfig 9878 Nothing 9051 9050 [(1, handler)]

main =
  startRicochet config [] $ do
    addr <- use hiddenDomain
    liftIO . print $ addr
    void awaitConnection

handler con =
  forever $ do
    p <- nextPacket con
    liftIO $ print p
