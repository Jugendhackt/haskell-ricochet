{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Ricochet.Types
  (Ricochet(..)) where

import           Control.Applicative    (Applicative (..))
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.State    (MonadState (..), StateT (..))
import           Network.Socket         (Socket ())


newtype Ricochet a = Ricochet { runRicochet :: StateT RicochetState IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadState RicochetState)

data Packet = MkPacket
  { _size       :: Int
  , _channelID  :: Int
  , _packetData :: ByteString }

makeLenses ''Packet

data RicochetState = MkRicochetState
  { _serverSocket :: Socket
  , _connections :: [Connection]
  }

makeLenses ''RicochetState

data Connection = MkConnection
  { _socket :: Socket
  }

makeLenses ''Connection
