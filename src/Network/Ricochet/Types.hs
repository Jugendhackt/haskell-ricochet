{-|
  Module:      Network.Ricochet.Types
  Description: Most of the type and lens definitions

"Network.Ricochet.Types" contains most of the definitions of the types and
lenses used throughout the package.
-}

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
  { _pSize       :: Word16     -- ^ The size of the whole packet
  , _pChannelID  :: Word16     -- ^ The channel the packet should be sent on
  , _pPacketData :: ByteString -- ^ The actual packet payload
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
data Connection = MkConnection
  { _cHandle         :: Handle         -- ^ The handle to send and receive signals on
  , _cChannels       :: [Channel]      -- ^ A list of the channels currently opened
  , _cInputBuffer    :: ByteString     -- ^ Buffered data that has not been parsed yet
  , _cConnectionRole :: ConnectionRole -- ^ The connection role of this side of the connection
  }

-- | Creates an initial 'Connection' from a Handle
makeConnection :: Handle -> ConnectionRole -> Connection
makeConnection handle role = -- Start out with only a Control Channel
  let channels = [MkChannel 0 $ MkChannelType "im.ricochet.control-channel"]
  in  MkConnection handle channels B.empty role

-- | Equality is defined by equality of the socket
instance Eq Connection where
  a == b = _cHandle a == _cHandle b

-- | Low level representation of a ricochet channel
data Channel = MkChannel
  { _cChannelID   :: Word16      -- ^ The ID of the channel
  , _cChannelType :: ChannelType -- ^ The type of the channel
  }

-- | The type of a channel is preliminarily represented by a ByteString for extensibility
data ChannelType = MkChannelType ByteString

-- | A contact, defined by his ID (TOR hidden service address without the .onion and the 'ricochet:' prefix) and his display name
data Contact = MkContact
  { _cName       :: String -- ^ The name assigned to the contact
  , _cRicochetID :: String -- ^ The ricochet ID of a user is their hidden service address without the ".onion"
  }

-- | ParserResult holds the result of a parser in a way
-- that is nice to handle within our library.
data ParserResult a = Success a ByteString
                    | Unfinished
                    | Failure

makeLenses ''Packet
makeLenses ''Connection
makeLenses ''Contact
