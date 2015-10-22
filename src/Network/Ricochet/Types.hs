{-# LANGUAGE TemplateHaskell #-}
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

-- | Function creating a packet with appropiate size from a Channel and a ByteString
makePacket :: Word16 -> ByteString -> Packet
makePacket chan bs = MkPacket (4 + (fromIntegral $ B.length bs)) chan bs

-- | Representation of a connection between two ricochet users
-- it consists of:
--  - open channels
--  - wether our ricochet instance is the client
-- Equality is defined by equality of the socket
data Connection = MkConnection
  { _cHandle      :: Handle
  , _cChannels    :: [Channel]
  , _cIsClient    :: Bool
  , _cInputBuffer :: ByteString
  }

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
