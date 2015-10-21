{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Ricochet
import           Network.Ricochet.Connection
import           Network.Ricochet.Monad

import           Control.Lens
import           Control.Monad               (when)
import           Control.Monad.State
import           Data.ByteString             (ByteString (), pack)
import qualified Data.Map                    as M
import           Network                     hiding (connectTo)
import           System.Environment          (getArgs)

main = do
  args <- getArgs
  when (length args /= 1) $ error "Usage: client <onion address>"
  state <- createState (PortNumber 9878)
  flip (runStateT . runRicochet) state $ do
    versions %= (M.insert 1 (\con -> do
      liftIO $ putStrLn "Version 1"
      forever $ do
        p <- nextPacket con
        liftIO . print $ p))
    con <- connectTo (head args) (PortNumber 9878)
    nextPacket con >>= liftIO . print
