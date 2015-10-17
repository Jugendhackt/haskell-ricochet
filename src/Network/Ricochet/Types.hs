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
  } deriving (Show)

data RicochetState = MkRicochetState
  { _serverSocket :: Socket
  , _connections  :: [Connection]
  } deriving (Show)

data Connection = MkConnection
  { _socket :: Socket
  } deriving (Show)

makeLenses ''Packet
makeLenses ''Connection
