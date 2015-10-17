{-# LANGUAGE OverloadedStrings #-}

module Network.Ricochet.Connection
  ( createState
  , awaitConnection
  , connectTo
  ) where

import           Network.Ricochet.Monad
import           Network.Ricochet.Types

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as B
import           Data.Monoid            ((<>))
import           Network                (PortID (..), accept, listenOn)
import           Network.Socks5         (socksConnectTo)

createState :: PortID -> IO RicochetState
createState port = do
  sock <- listenOn port
  return $ MkRicochetState sock [] [] (PortNumber 9050)

awaitConnection :: Ricochet ()
awaitConnection = do
  sock <- use serverSocket
  (handle, _, _) <- liftIO $ accept sock
  let connection = MkConnection handle [MkChannel 0 $ MkChannelType "im.ricochet.control-channel"] False B.empty
  connections %= (<> [connection])

connectTo :: String -> PortID -> Ricochet Connection
connectTo domain port = do
  torPort <- use socksPort
  handle <- liftIO $ socksConnectTo "localhost" torPort domain port
  let connection = MkConnection handle [MkChannel 0 $ MkChannelType "im.ricochet.control-channel"] True B.empty
  connections %= (<> [connection])
  return connection
