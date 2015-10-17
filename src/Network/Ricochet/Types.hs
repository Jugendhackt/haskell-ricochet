{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Network.Ricochet.Types
  (Ricochet(..)) where

import           Control.Applicative    (Applicative (..))
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.State    (MonadState (..), StateT (..))
import           Data.ByteString        (ByteString ())
import           Data.Word              (Word16)
import           Network.Socket         (Socket ())


newtype Ricochet a = Ricochet { runRicochet :: StateT RicochetState IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadState RicochetState)

data Packet = MkPacket
  { _size       :: Word16
  , _channelID  :: Word16
  , _packetData :: ByteString
  }

data RicochetState = MkRicochetState
  { _serverSocket :: Socket
  , _connections  :: [Connection]
  }

data Connection = MkConnection
  { _socket :: Socket
  }

makeLenses ''Packet
makeLenses ''RicochetState
makeLenses ''Connection
