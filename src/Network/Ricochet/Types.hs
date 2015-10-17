{-# LANGUAGE TemplateHaskell #-}
module Network.Ricochet.Types where

import           Control.Lens
import           Data.ByteString        (ByteString ())
import           Data.Word              (Word16)
import           Network.Socket         (Socket ())

-- | Low level representation of a ricochet packet
data Packet = MkPacket
  { _pSize       :: Word16
  , _pChannelID  :: Word16
  , _pPacketData :: ByteString
  } deriving (Show)

-- | Representation of a connection between two ricochet users
-- it consists of:
--  - open channels
--  - wether our ricochet instance is the client
-- Equality is defined by equality of the socket
data Connection = MkConnection
  { _cSocket   :: Socket
  , _cChannels :: [Channel]
  , _cIsClient :: Bool
  }

instance Eq Connection where
  a == b = _cSocket a == _cSocket b

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

makeLenses ''Packet
makeLenses ''Connection
makeLenses ''Contact
