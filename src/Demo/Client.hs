{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Ricochet
import           Network.Ricochet.Connection
import           Network.Ricochet.Monad
import           Network.Ricochet.Types

import           Control.Concurrent          (threadDelay)
import           Control.Lens
import           Control.Monad               (when, void)
import           Control.Monad.State
import           Data.ByteString             (ByteString (), pack)
import qualified Data.Map                    as M
import           Network                     hiding (accept, connectTo)
import           System.Environment          (getArgs)

config = RicochetConfig 9879 Nothing 9051 9050 [(1, handler)]

main = do
  args <- getArgs
  when (length args /= 1) $ error "Usage: client <onion address>"
  startRicochet config [] $ do
    addr <- use hiddenDomain
    liftIO . print $ addr
    con <- connectTo (head args) (PortNumber 9878)
    liftIO . putStrLn $ "Connected!"
    nextPacket con >>= liftIO . print

handler con =
  forever $ do
    p <- nextPacket con
    liftIO . print $ p
