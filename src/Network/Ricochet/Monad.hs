{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Ricochet.Monad
  ( Ricochet (..)
  , RicochetState (..)
  , serverSocket, connections
  , peekPacket, nextPacket
  ) where

import           Network.Ricochet.Types
import           Network.Ricochet.Protocol.Lowest

import           Control.Applicative    (Applicative (..))
import           Control.Concurrent     (threadDelay)
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.State    (MonadState (..), StateT (..))
import           Data.ByteString        (ByteString ())
import qualified Data.ByteString        as B
import           Data.Monoid            ((<>))
import           Data.Word              (Word16)
import           Network.Socket         (Socket ())
import           System.IO              (Handle (), hSetBuffering, BufferMode(..))

newtype Ricochet a = Ricochet { runRicochet :: StateT RicochetState IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadState RicochetState)

data RicochetState = MkRicochetState
  { _serverSocket :: Socket
  , _connections  :: [Connection]
  , _contactList  :: [Contact]
  }

makeLenses ''RicochetState

-- | Checks if a complete packet is available on the given connection, and if
-- so, reads and returns it.
peekPacket :: Connection -> Ricochet (Maybe Packet)
peekPacket con = do
  readBytes <- liftIO $ B.hGetNonBlocking (con ^. cHandle) max
  inputBuffer <- con' . cInputBuffer <%= (<> readBytes)
  case parsePacket inputBuffer of
    Just (packet, bs) -> do
      -- FIXME: Should be: con' . cInputBuffer .= bs
      con' . cInputBuffer <%= (const bs)
      return $ Just packet
    Nothing -> return Nothing
  where max = fromIntegral (maxBound :: Word16)
        con' = connections . traversed . filtered (== con)

nextPacket :: Connection -> Ricochet Packet
nextPacket con = do
  maybePacket <- peekPacket con
  case maybePacket of
    Just pkt -> return pkt
    Nothing -> liftIO (threadDelay delay) >> nextPacket con
  where delay = round $ 10 ** 4

sendPacket :: Connection -> Packet -> Ricochet ()
sendPacket con pkt = liftIO . B.hPutStr (con ^. cHandle) $ dumpPacket pkt
