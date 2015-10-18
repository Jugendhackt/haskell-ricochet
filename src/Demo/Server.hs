{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Ricochet
import           Network.Ricochet.Connection
import           Network.Ricochet.Monad

import           Control.Monad.State
import           Data.ByteString             (ByteString (), pack)
import           Data.Map                    (empty)
import           Network                     hiding (connectTo)

main = do
  state <- createState (PortNumber 9878)
  flip (runStateT . runRicochet) state $ do
    con <- awaitConnection
    packet <- nextPacket con
    liftIO . print $ packet
