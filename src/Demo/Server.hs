{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Ricochet
import           Network.Ricochet.Connection
import           Network.Ricochet.Monad
import           Network.Ricochet.Types

import           Control.Lens
import           Control.Monad.State
import           Data.ByteString             (ByteString (), pack)
import qualified Data.Map                    as M
import           Data.Monoid                 ((<>))
import           Network                     hiding (connectTo)

main = do
  state <- createState (PortNumber 9878)
  flip (runStateT . runRicochet) state $ do
    versions %= M.insert 1 (\_ -> return ())
    con <- awaitConnection
    forever $ do
      p <- nextPacket con
      liftIO $ print p
