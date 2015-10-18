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
    con <- awaitConnection
    packet <- nextPacket con
    liftIO . print $ packet
