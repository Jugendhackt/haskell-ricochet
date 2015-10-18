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
    versions %= (M.insert 20 (\con -> do
      liftIO $ putStrLn "Version 20"))
    con <- awaitConnection
    sendByteString con 0x00 "Jeremy ist total cute!"
    forever $ do
      p <- nextPacket con
      sendByteString con 0x00 $ "PONG " <> (p ^. pPacketData)
      liftIO $ print p
