{-# LANGUAGE TemplateHaskell #-}
module Network.Ricochet.Types
  ( Packet(..)
  , Connection(..)
  , size, channelID, packetData, socket) where

import           Control.Lens
import           Data.ByteString        (ByteString ())
import           Data.Word              (Word16)
import           Network.Socket         (Socket ())

data Packet = MkPacket
  { _size       :: Word16
  , _channelID  :: Word16
  , _packetData :: ByteString
  }

data Connection = MkConnection
  { _socket :: Socket
  }

makeLenses ''Packet
makeLenses ''Connection
