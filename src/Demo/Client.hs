{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Ricochet
import           Network.Ricochet.Connection
import           Network.Ricochet.Monad

import           Control.Monad.State
import           Control.Lens
import           Data.ByteString             (ByteString (), pack)
import qualified Data.Map                    as M
import           Network                     hiding (connectTo)

main = do
  state <- createState (PortNumber 9878)
  flip (runStateT . runRicochet) state $ do
    versions %= (M.insert 20 (\con -> do
      liftIO $ putStrLn "Version 20"))
    con <- connectTo "b362vohgp2nccztw.onion" (PortNumber 9878)
    nextPacket con >>= liftIO . print
    sendByteString con 0x00 "12345"
    sendByteString con 0x00 "Zweites Paket huhu"
    nextPacket con >>= liftIO . print
    nextPacket con >>= liftIO . print
