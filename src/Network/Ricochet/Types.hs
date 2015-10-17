{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Ricochet.Types
  (Ricochet(..)) where

import           Control.Applicative    (Applicative (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..), ReaderT (..))
import           Network.Socket         (Socket ())


newtype Ricochet a = Ricochet { runRicochet :: ReaderT Socket IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader Socket)
