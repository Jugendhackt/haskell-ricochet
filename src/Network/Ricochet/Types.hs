{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Ricochet.Types where

import           Control.Lens
import           Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import           Data.Word       (Word16)
import           Network.Socket  (Socket ())
import           System.IO       (Handle ())

-- | Low level representation of a ricochet packet
data Packet = MkPacket
  { _pSize       :: Word16
  , _pChannelID  :: Word16
  , _pPacketData :: ByteString
  } deriving (Show)

-- | Creates a packet with appropriate size from a Channel and a ByteString
makePacket :: Word16     -- ^ ID of the channel the packet should be sent on
           -> ByteString -- ^ The ByteString to be sent
           -> Packet     -- ^ Returns a sendable packet
makePacket chan bs = MkPacket (4 + fromIntegral (B.length bs)) chan bs

-- | The role of a peer in a Connection
data ConnectionRole = Client | Server deriving (Eq, Show)

-- | Representation of a connection between two ricochet users
-- it consists of:
--  - open channels
--  - wether our ricochet instance is the client
-- Equality is defined by equality of the socket
data Connection = MkConnection
  { _cHandle         :: Handle
  , _cChannels       :: [Channel]
  , _cInputBuffer    :: ByteString
  , _cConnectionRole :: ConnectionRole
  }

-- | Creates an initial 'Connection' from a Handle
makeConnection :: Handle -> ConnectionRole -> Connection
makeConnection handle role = -- Start out with only a Control Channel
  let channels = [MkChannel 0 $ MkChannelType "im.ricochet.control-channel"]
  in  MkConnection handle channels B.empty role

instance Eq Connection where
  a == b = _cHandle a == _cHandle b

-- | Low level representation of a ricochet channel
data Channel = MkChannel
  { _cChannelID   :: Word16
  , _cChannelType :: ChannelType
  }

-- | The type of a channel is preliminarily represented by a ByteString for extensibility
data ChannelType = MkChannelType ByteString

-- | A contact of our user, defined by his ID (TOR hidden service address without the .onion and the 'ricochet:' prefix) and his display name
data Contact = MkContact
  { _cName       :: String
  , _cRicochetID :: String
  }

-- | ParserResult holds the result of a parser in a way
-- that is nice to handle within our library.
data ParserResult a = Success a ByteString
                    | Unfinished
                    | Failure

makeLenses ''Packet
makeLenses ''Connection
makeLenses ''Contact
