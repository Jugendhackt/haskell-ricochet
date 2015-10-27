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

import Data.Map (fromList)
import Data.Base32String (Base32String)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Network (PortID(..), Socket, listenOn)
import Control.Lens
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)
import Network.Anonymous.Tor (mapOnion, withSession)

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
              -> (Base32String -> Ricochet ()) -- ^ The action will be supplied the address of the
                                               --   hidden service
              -> IO ()
startRicochet lPort@(PortNumber listenPort) key (PortNumber ctrlPort) contacts socksPort versions action = do
  let listenInt = fromIntegral listenPort
      ctrlInt   = fromIntegral ctrlPort
  listenSock <- listenOn lPort
  void . withSession ctrlInt $ \ctrlSock -> do
    address <- mapOnion ctrlSock listenInt listenInt False key
    startRicochet' listenSock contacts socksPort versions . action $ address
startRicochet _ _ _ _ _ _ _ = error "network-anonymous-tor currently only accepts Integers as ports, sorry"

-- | Start an action inside the Ricochet monad.
--
--   This function assumes the listening socket was already configured to be a
--   hidden service via the torrc or some other method.
--
--   DON'T use this function if that's not the case.
startRicochet' :: Socket   -- ^ Socket to listen on
              -> [Contact] -- ^ A list of known contacts
              -> PortID    -- ^ The port of the Tor SOCKS5 proxy
              -> [(Word8, Connection -> Ricochet ())] -- ^ A list of version identifiers and their
                                                      --   corresponding 'Connection' handlers
              -> Ricochet () -- ^ The action to execute
              -> IO ()
startRicochet' sock contacts soxPort versions action =
  let versions' = fromList versions
      state     = MkRicochetState sock [] contacts soxPort versions'
  in flip evalStateT state . runRicochet $ action
