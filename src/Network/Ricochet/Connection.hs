{-|
  Module:   Network.Ricochet.Connection
  Description: Implementation of everything related to initiating 'Connection's

"Network.Ricochet.Connection" implements the opening of 'Connection's as well
as the first few steps of the protocol.
-}

{-# LANGUAGE OverloadedStrings #-}

module Network.Ricochet.Connection
  ( awaitConnection
  , connectTo
  ) where

import           Network.Ricochet.Monad   (Ricochet, closeConnection,
                                           connections, serverSocket, socksPort, versions)
import           Network.Ricochet.Types   (Connection, ConnectionRole(..),
                                           ParserResult(..), cHandle,
                                           cInputBuffer, makeConnection)
import           Network.Ricochet.Protocol.Version (Versions, dumpIntroduction,
                                                    parseIntroduction)

import           Control.Arrow            (first)
import           Control.Concurrent       (threadDelay)
import           Control.Lens             ((%=), (^.), filtered, use)
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (liftIO)
import           Data.ByteString          (ByteString ())
import qualified Data.ByteString          as B
import qualified Data.Map                 as M
import           Data.Maybe               (fromJust)
import           Data.Monoid              ((<>))
import           Network                  (PortID (..), accept, listenOn)
import           Network.Socks5           (socksConnectTo)
import           System.IO                (BufferMode (..), Handle (),
                                           hSetBuffering)

-- | Waits until a new peer connects to initiate a connection
awaitConnection :: Ricochet Connection
awaitConnection = do
  sock <- use serverSocket
  (handle, _, _) <- liftIO $ accept sock
  initiateConnection handle Server

-- | Connects to a peer through the Tor network
connectTo :: String              -- ^ Tor hidden service identifier to connect to
          -> PortID              -- ^ Port to connect to
          -> Ricochet Connection -- ^ Returns the established connection
connectTo domain port = do
  torPort <- use socksPort
  handle <- liftIO $ socksConnectTo "localhost" torPort domain port
  initiateConnection handle Client

-- | Initiate a newly made connection.  This includes the version negotiation
--   and running the corresponding version handler
initiateConnection :: Handle              -- ^ Handle corresponding to the connection to the peer
                   -> ConnectionRole      -- ^ The connection role of this side of the connection
                   -> Ricochet Connection -- ^ Returns the finished 'Connection'
initiateConnection handle role = do
  connection <- createConnection handle role
  case role of
    Client -> offerVersions connection
    Server -> pickVersion connection
  return connection

-- | Creates a new 'Connection' and adds it to the list of open Connections
createConnection :: Handle              -- ^ Handle corresponding to the connection to the peer
                 -> ConnectionRole      -- ^ The connection role of this side of the connection
                 -> Ricochet Connection -- ^ Returns the finished 'Connection'
createConnection handle role = do
  -- Create the actual connection structure
  let connection = makeConnection handle role
  -- Disable buffering
  liftIO $ hSetBuffering handle NoBuffering
  -- Add it to the list of open Connections
  connections %= (<> [connection])
  return connection

-- | Offers the available versions to the newly connected peer and starts the
--   handler corresponding to the one the peer picked
offerVersions :: Connection -> Ricochet ()
offerVersions connection = do
  availableVersions <- use versions
  -- Send the available versions
  liftIO . B.hPutStr (connection ^. cHandle) $ dumpIntroduction availableVersions
  response <- liftIO $ B.hGet (connection ^. cHandle) 1
  let choice = head $ B.unpack response
  -- If the choice is valid
  if choice `M.member` availableVersions
    -- Start the handler corresponding to the negotiated version
    then availableVersions M.! choice $ connection
    else closeConnection connection

-- | Picks a version supported by the peer and starts the corresponding handler
pickVersion :: Connection -> Ricochet ()
pickVersion connection = do
  availableVersions <- use versions
  response <- liftIO . awaitIntroMessage availableVersions $ connection ^. cHandle
  case fmap (first M.toList) response of
    -- No matching versions
    Just ([], rest) -> do
      liftIO . B.hPutStr (connection ^. cHandle) $ B.singleton 0xFF
      closeConnection connection
    -- The versions match
    Just (handlers, rest) -> do
      -- Always choose the latest version
      let chosen = maximum (fmap fst handlers)
      liftIO . B.hPutStr (connection ^. cHandle) $ B.singleton chosen
      -- Append the rest of the response to the inputBuffer
      connections . traverse . filtered (== connection) . cInputBuffer %= (<> rest)
      -- Start the chosen handler
      (fromJust $ lookup chosen handlers) connection
    Nothing -> closeConnection connection

-- | Waits for the peer to send the introductory message
awaitIntroMessage :: Versions -> Handle -> IO (Maybe (Versions, ByteString))
awaitIntroMessage vers handle = do
  introMessage <- B.hGetNonBlocking handle 300
  case parseIntroduction vers introMessage of
   Success map b -> return $ Just (map, b)
   Unfinished    -> liftIO (threadDelay delay) >> awaitIntroMessage vers handle
   Failure       -> return Nothing
  where delay = round $ 10 ** 4
