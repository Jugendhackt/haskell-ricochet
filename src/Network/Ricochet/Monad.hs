
{- |
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
  , startRicochet, RicochetConfig (..)
  ) where

import Network.Ricochet.Protocol.Lowest
import Network.Ricochet.Types
import Network.Ricochet.Util

import Control.Applicative (Applicative (..))
import Control.Concurrent (threadDelay)
import Control.Lens ((<%=), (%=), (^.), makeLenses, use)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (MonadIO (..), liftIO)
import Control.Monad.State (MonadState (..), StateT (..), evalStateT)
import Data.Base32String.Default (toText)
import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import Data.List (delete)
import Data.Map (Map (), lookup, empty, fromList)
import Data.Monoid ((<>))
import Data.Text (toLower)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8, Word16)
import Network.Socket (Socket (), PortNumber (..), socket, bind,
                       inet_addr, SocketType (..), defaultProtocol,
                       SockAddr (..), Family (..))
import Network.BSD (getServicePortNumber)
import Network.Anonymous.Tor (mapOnion, withSession)
import System.IO (BufferMode (..), Handle (), hSetBuffering, hClose)


-- | The Ricochet Monad which allows stateful network computations
newtype Ricochet a = Ricochet { runRicochet :: StateT RicochetState IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadState RicochetState)

-- | RicochetState is the state necessary for Ricochet
data RicochetState = MkRicochetState
  { _serverSocket :: Socket                                -- ^ The socket listening for new peers
  , _hiddenDomain :: ByteString                            -- ^ The domain of our hidden service
  , _connections  :: [Connection]                          -- ^ A list of the current open connections
  , _contactList  :: [Contact]                             -- ^ A list of known contacts
  , _socksPort    :: PortNumber                            -- ^ The port of the local Tor SOCKS proxy
  , _versions     :: Map Word8 (Connection -> Ricochet ()) -- ^ A map mapping version numbers to handlers
  }

-- | Lenses for RicochetState
--
makeLenses ''RicochetState


-- | Checks if a complete packet is available on the given connection, and if
--   so, reads and returns it.
--
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
    -- Todo: quit connection for unreadable packages?
  where maxPacketSize = fromIntegral (maxBound :: Word16)
        con'          = connections . look con
        chan' p       = con' . cChannels . lookWith cChannelID (p ^. pChannelID)


-- | Waits for a complete packet to arrive and returns it
--
nextPacket :: Connection -> Ricochet Packet
nextPacket con = do
  maybePacket <- peekPacket con
  case maybePacket of
    Just pkt -> return pkt
    Nothing -> liftIO (threadDelay delay) >> nextPacket con
  where delay = 10 ^ 6 -- μs -> 1s


-- | Sends a Packet to a connected User
--
sendPacket :: Connection -> Packet -> Ricochet ()
sendPacket con pkt = liftIO . B.hPutStr (con ^. cHandle) $ dumpPacket pkt


-- | Packs a ByteString into Packets and sends it through the given Connection
--
sendByteString :: Connection -- ^ The Connection through which to send the ByteString
               -> Word16     -- ^ The ID of the channel the ByteString should be sent on
               -> ByteString -- ^ The ByteString to be sent
               -> Ricochet ()
sendByteString con chan bs = mapM_ (sendPacket con) $ splitIntoPackets chan bs


-- | Closes a connection and removes it from the list of connections
--
closeConnection :: Connection -> Ricochet ()
closeConnection connection = do
  connections %= delete connection
  liftIO . hClose $ connection ^. cHandle

data RicochetConfig = RicochetConfig
  { rcPort        :: PortNumber       -- ^ Port to listen on
  , rcPrivKey     :: Maybe ByteString -- ^ RSA1024 private key in base64 encoding
  , rcControlPort :: PortNumber       -- ^ The Tor control port
  , rcSocksPort   :: PortNumber       -- ^ The port of the Tor SOCKS5 proxy
  , rcHandlers    :: [(Word8, Connection -> Ricochet ())] -- ^ A list of version identifiers and their
                                                          --   corresponding 'Connection' handlers
  }


-- | Start an action inside the Ricochet monad.
--
--   This function uses a Tor control socket to create the hidden service.
--   If no key is supplied, the hidden service will be random
--
startRicochet :: RicochetConfig
              -> [Contact]        -- ^ A list of known 'Contact's
              -> Ricochet ()
              -> IO ()
startRicochet config contacts action = do
  let key       = rcPrivKey config
      listenInt = fromIntegral lPort
      ctrlInt   = fromIntegral . rcControlPort $ config
      lPort     = rcPort config
      cPort     = rcSocksPort config

  listenSock <- socket AF_INET Stream defaultProtocol
  addr <- inet_addr "127.0.0.1"
  bind listenSock (SockAddrInet listenInt addr)
  void . withSession ctrlInt $ \ctrlSock -> do
      address <- encodeUtf8 . toLower . toText <$> mapOnion ctrlSock listenInt listenInt False key
      startRicochet' listenSock address contacts (rcSocksPort config) (rcHandlers config) action


-- | Start an action inside the Ricochet monad.
--
--   This function assumes the listening socket was already configured to be a
--   hidden service via the torrc or some other method.
--
--   DON'T use this function if that's not the case.
--
startRicochet' :: Socket      -- ^ Socket to listen on
              -> ByteString   -- ^ Domain of the already configured hidden service
              -> [Contact]    -- ^ A list of known contacts
              -> PortNumber   -- ^ The port of the Tor SOCKS5 proxy
              -> [(Word8, Connection -> Ricochet ())] -- ^ A list of version identifiers and their
                                                      --   corresponding 'Connection' handlers
              -> Ricochet ()  -- ^ The action to execute
              -> IO ()
startRicochet' sock address contacts soxPort versions action =
  let versions' = fromList versions
      state     = MkRicochetState sock address [] contacts soxPort versions'
  in flip evalStateT state . runRicochet $ action
