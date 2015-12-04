module Network.Ricochet where

import Network.Ricochet.Connection
import Network.Ricochet.Monad
import Network.Ricochet.Protocol.Lowest
import Network.Ricochet.Protocol.Protobuf
import Network.Ricochet.Protocol.Protobuf.AuthHiddenService
import Network.Ricochet.Protocol.Protobuf.ControlChannel
import Network.Ricochet.Types
import Network.Ricochet.Util
import Network.Ricochet.Version

import           Data.Map                         (fromList)
import           Data.Base32String.Default        (toText)
import           Data.ByteString                  (ByteString)
import           Data.Text                        (toLower)
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Word                        (Word8)
import           Network                          (PortID(..), Socket, listenOn)
import           Control.Lens
import           Control.Monad                    (void, forever)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.State              (evalStateT)
import           Network.BSD                      (getServicePortNumber)
import           Network.Anonymous.Tor            (mapOnion, withSession)

-- | Start an action inside the Ricochet monad.
--
--   This function uses a Tor control socket to create the hidden service.
--   If no key is supplied, the hidden service will be random
startRicochet :: PortID           -- ^ Port to listen on
              -> Maybe ByteString -- ^ RSA1024 private key in base64 encoding
              -> PortID           -- ^ The Tor control port
              -> [Contact]        -- ^ A list of known 'Contact's
              -> PortID           -- ^ The port of the Tor SOCKS5 proxy
              -> [(Word8, Connection -> Ricochet ())] -- ^ A list of version identifiers and their
                                                      --   corresponding 'Connection' handlers
              -> Ricochet ()
              -> IO ()
-- Convert the lPort to a PortNumber, if it's a service
startRicochet (Service s) key ctrlPort contacts socksPort versions action = do
  portNum <- getServicePortNumber s
  startRicochet (PortNumber portNum) key ctrlPort contacts socksPort versions action
-- Convert the ctrlPort to a PortNumber, if it's a service
startRicochet lPort key (Service s) contacts socksPort versions action = do
  portNum <- getServicePortNumber s
  startRicochet lPort key (PortNumber portNum) contacts socksPort versions action
startRicochet lPort@(PortNumber listenPort) key (PortNumber ctrlPort) contacts socksPort versions action = do
  let listenInt = fromIntegral listenPort
      ctrlInt   = fromIntegral ctrlPort
  listenSock <- listenOn lPort
  void . withSession ctrlInt $ \ctrlSock -> do
    address <- encodeUtf8 . toLower . toText <$> mapOnion ctrlSock listenInt listenInt False key
    startRicochet' listenSock address contacts socksPort versions action
startRicochet _ _ _ _ _ _ _ = error "Unfortunately, startRicochet doesn't support UNIX sockets"

-- | Start an action inside the Ricochet monad.
--
--   This function assumes the listening socket was already configured to be a
--   hidden service via the torrc or some other method.
--
--   DON'T use this function if that's not the case.
startRicochet' :: Socket      -- ^ Socket to listen on
              -> ByteString   -- ^ Domain of the already configured hidden service
              -> [Contact]    -- ^ A list of known contacts
              -> PortID       -- ^ The port of the Tor SOCKS5 proxy
              -> [(Word8, Connection -> Ricochet ())] -- ^ A list of version identifiers and their
                                                      --   corresponding 'Connection' handlers
              -> Ricochet ()  -- ^ The action to execute
              -> IO ()
startRicochet' sock address contacts soxPort versions action =
  let versions' = fromList versions
      state     = MkRicochetState sock address [] contacts soxPort versions'
  in flip evalStateT state . runRicochet $ action
