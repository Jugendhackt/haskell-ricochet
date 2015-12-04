{-|
  Module:      Network.Ricochet.Monad
  Description: Implementation of the 'Ricochet'-Monad

"Network.Ricochet.Monad" contains the definition and implementation of the
'Ricochet'-Monad, as well as some useful functions in it.
-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
module Network.Ricochet.Monad
  ( Ricochet (..)
  , RicochetState (..)
  , serverSocket, hiddenDomain
  , connections, peekPacket
  , nextPacket, socksPort
  , versions, sendPacket
  , sendByteString, closeConnection
  ) where

import           Network.Ricochet.Protocol.Lowest
import           Network.Ricochet.Types
import           Network.Ricochet.Util

import           Control.Applicative              (Applicative (..))
import           Control.Concurrent               (threadDelay)
import           Control.Lens                     ((<%=), (%=), (^.),
                                                   makeLenses, use)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.State              (MonadState (..), StateT (..))
import           Data.ByteString                  (ByteString ())
import qualified Data.ByteString                  as B
import           Data.List                        (delete)
import           Data.Map                         (Map (), lookup, empty)
import           Data.Monoid                      ((<>))
import           Data.Word                        (Word8, Word16)
import           Network                          (PortID (..))
import           Network.Socket                   (Socket ())
import           System.IO                        (BufferMode (..), Handle (),
                                                   hSetBuffering, hClose)

-- | The Ricochet Monad which allows all stateful network computations we need to do
newtype Ricochet a = Ricochet { runRicochet :: StateT RicochetState IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadState RicochetState)

-- | RicochetState is the state necessary for Ricochet
data RicochetState = MkRicochetState
  { _serverSocket :: Socket                                -- ^ The socket listening for new peers
  , _hiddenDomain :: ByteString                            -- ^ The domain of our hidden service
  , _connections  :: [Connection]                          -- ^ A list of the current open connections
  , _contactList  :: [Contact]                             -- ^ A list of known contacts
  , _socksPort    :: PortID                                -- ^ The port of the local Tor SOCKS proxy
  , _versions     :: Map Word8 (Connection -> Ricochet ()) -- ^ A map mapping version numbers to handlers
  }

makeLenses ''RicochetState

-- | Checks if a complete packet is available on the given connection, and if
--   so, reads and returns it.
peekPacket :: Connection -> Ricochet (Maybe Packet)
peekPacket con = do
  readBytes <- liftIO $ B.hGetNonBlocking (con ^. cHandle) maxPacketSize
  -- Append the read bytes to the input buffer
  inputBuffer <- con' . cInputBuffer <%= (<> readBytes)
  -- Try parsing a full packet and return it on success
  case parsePacket inputBuffer of
    Success packet bs -> do
      -- Remove the parsed portion from the inputBuffer
      -- FIXME: Should be: con' . cInputBuffer .= bs
      con' . cInputBuffer <%= const bs
      return $ Just packet
    Unfinished -> return Nothing
    Failure    -> return Nothing
  where maxPacketSize = fromIntegral (maxBound :: Word16)
        con'          = connections . look con
        chan' p       = con' . cChannels . lookWith cChannelID (p ^. pChannelID)

-- | Waits for a complete packet to arrive and returns it
nextPacket :: Connection -> Ricochet Packet
nextPacket con = do
  maybePacket <- peekPacket con
  case maybePacket of
    Just pkt -> return pkt
    Nothing -> liftIO (threadDelay delay) >> nextPacket con
  where delay = round $ 10 ** 6

-- | Sends a Packet to a connected User
sendPacket :: Connection -> Packet -> Ricochet ()
sendPacket con pkt = liftIO . B.hPutStr (con ^. cHandle) $ dumpPacket pkt

-- | Packs a ByteString into Packets and sends it through the given Connection
sendByteString :: Connection -- ^ The Connection through which to send the ByteString
               -> Word16     -- ^ The ID of the channel the ByteString should be sent on
               -> ByteString -- ^ The ByteString to be sent
               -> Ricochet ()
sendByteString con chan bs = mapM_ (sendPacket con) $ splitIntoPackets chan bs

-- | Closes a connection and removes it from the list of connections
closeConnection :: Connection -> Ricochet ()
closeConnection connection = do
  connections %= delete connection
  liftIO . hClose $ connection ^. cHandle
