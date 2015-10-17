{-# LANGUAGE OverloadedStrings #-}

module Network.Ricochet.Connection
  (connectTo) where

import Network.Ricochet.Monad
import Network.Ricochet.Types

import Control.Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Network        (PortID(..))
import Network.Socks5 (socksConnectTo)

connectTo :: String -> PortID -> Ricochet Connection
connectTo domain port = do
  torPort <- use socksPort
  handle <- liftIO $ socksConnectTo "localhost" torPort domain port
  return $ MkConnection handle [MkChannel 0 $ MkChannelType "im.ricochet.control-channel"] True B.empty
