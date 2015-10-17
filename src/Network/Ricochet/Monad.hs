{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Network.Ricochet.Monad
  ( Ricochet (..)
  , RicochetState (..)
  , serverSocket, connections
  , peekPacket, nextPacket
  , socksPort, versions
  ) where

import           Network.Ricochet.Protocol.Lowest
import           Network.Ricochet.Types

import           Control.Applicative              (Applicative (..))
import           Control.Concurrent               (threadDelay)
import           Control.Lens
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.State              (MonadState (..), StateT (..))
import           Data.ByteString                  (ByteString ())
import qualified Data.ByteString                  as B
import           Data.Map                         (Map (), lookup, empty)
import           Data.Monoid                      ((<>))
import           Data.Word                        (Word8, Word16)
import           Network                          (PortID (..))
import           Network.Socket                   (Socket ())
import           System.IO                        (BufferMode (..), Handle (),
                                                   hSetBuffering)

-- | The Ricochet Monad which allows all stateful network computations we need to do
newtype Ricochet a = Ricochet { runRicochet :: StateT RicochetState IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadState RicochetState)

-- | RicochetState is the state necessary for Ricochet
data RicochetState = MkRicochetState
  { _serverSocket :: Socket
  , _connections  :: [Connection]
  , _contactList  :: [Contact]
  , _socksPort    :: PortID
  , _versions     :: Map Word8 (Connection -> Ricochet ())
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

-- | Waits for a complete packet to arrive and returns it
nextPacket :: Connection -> Ricochet Packet
nextPacket con = do
  maybePacket <- peekPacket con
  case maybePacket of
    Just pkt -> return pkt
    Nothing -> liftIO (threadDelay delay) >> nextPacket con
  where delay = round $ 10 ** 4

-- | Sends a Packet to a connected User
sendPacket :: Connection -> Packet -> Ricochet ()
sendPacket con pkt = liftIO . B.hPutStr (con ^. cHandle) $ dumpPacket pkt

-- | Packs a ByteString into Packets and sends it through the given Connection
sendByteString :: Connection -> Word16 -> ByteString -> Ricochet ()
sendByteString con chan bs = mapM_ (sendPacket con) $ splitInPackets chan bs
