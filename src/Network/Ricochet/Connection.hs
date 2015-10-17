{-# LANGUAGE OverloadedStrings #-}

module Network.Ricochet.Connection
  ( createState
  , awaitConnection
  , connectTo
  ) where

import           Network.Ricochet.Monad
import           Network.Ricochet.Types
import           Network.Ricochet.Version

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as B
import qualified Data.Map               as M
import           Data.Monoid            ((<>))
import           Network                (PortID (..), accept, listenOn)
import           Network.Socks5         (socksConnectTo)
import           System.IO              (Handle ())

createState :: PortID -> IO RicochetState
createState port = do
  sock <- listenOn port
  return $ MkRicochetState sock [] [] (PortNumber 9050) defaultVersions

defaultVersions :: Versions
defaultVersions = M.empty

awaitConnection :: Ricochet Connection
awaitConnection = do
  sock <- use serverSocket
  (handle, _, _) <- liftIO $ accept sock
  initConnection handle False

connectTo :: String -> PortID -> Ricochet Connection
connectTo domain port = do
  torPort <- use socksPort
  handle <- liftIO $ socksConnectTo "localhost" torPort domain port
  initConnection handle True

initConnection :: Handle -> Bool -> Ricochet Connection
initConnection handle isClientSide = do
  let connection = MkConnection handle [MkChannel 0 $ MkChannelType "im.ricochet.control-channel"] isClientSide B.empty
  connections %= (<> [connection])
  -- TODO: Introduction Message
  return connection
