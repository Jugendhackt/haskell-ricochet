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

main =
  startRicochet (PortNumber 9878) Nothing (PortNumber 9051) [] (PortNumber 9050) [(1, handler)] $ do
    addr <- use hiddenDomain
    liftIO . print $ addr
    void awaitConnection

handler con =
  forever $ do
    p <- nextPacket con
    liftIO $ print p
