{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Network.Ricochet.Monad
  ( Ricochet (..)
  , RicochetState (..)
  , serverSocket, connections
  ) where

import           Network.Ricochet.Types

import           Control.Applicative    (Applicative (..))
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.State    (MonadState (..), StateT (..))
import           Network.Socket         (Socket ())

newtype Ricochet a = Ricochet { runRicochet :: StateT RicochetState IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadState RicochetState)

data RicochetState = MkRicochetState
  { _serverSocket :: Socket
  , _connections  :: [Connection]
  , _contactList  :: [Contact]
  }

makeLenses ''RicochetState
