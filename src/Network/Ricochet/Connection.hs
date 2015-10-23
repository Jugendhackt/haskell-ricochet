{-# LANGUAGE OverloadedStrings #-}

module Network.Ricochet.Connection
  ( createState
  , awaitConnection
  , connectTo
  ) where

import           Network.Ricochet.Monad
import           Network.Ricochet.Types
import           Network.Ricochet.Version

import           Control.Arrow            (first)
import           Control.Concurrent       (threadDelay)
import           Control.Lens
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

-- | Creates a new RicochetState listening on the supplied port
createState :: PortID -> IO RicochetState
createState port = do
  sock <- listenOn port
  return $ MkRicochetState sock [] [] (PortNumber 9050) M.empty

-- | Waits until a new peer connects to initiae a connection
awaitConnection :: Ricochet Connection
awaitConnection = do
  sock <- use serverSocket
  (handle, _, _) <- liftIO $ accept sock
  initiateConnection handle False

-- | Connects to a peer through the Tor network
connectTo :: String              -- ^ Tor hidden service identifier to connect to
          -> PortID              -- ^ Port to connect to
          -> Ricochet Connection -- ^ Returns the established connection
connectTo domain port = do
  torPort <- use socksPort
  handle <- liftIO $ socksConnectTo "localhost" torPort domain port
  initiateConnection handle False

-- | Initiate a newly made connection
initiateConnection :: Handle              -- ^ Handle corresponding to the connection to the peer
                   -> Bool                -- ^ Wether this side accepted the connection
                   -> Ricochet Connection -- ^ Returns the finished 'Connection'
initiateConnection handle isClientSide = do
  connection <- createConnection handle isClientSide
  if isClientSide
    then offerVersions connection
    else pickVersion connection
  return connection

-- | Creates a new 'Connection' and adds it to the list of open Connections
createConnection :: Handle              -- ^ Handle corresponding to the connection to the peer
                 -> Bool                -- ^ Wether this side accepted the connection
                 -> Ricochet Connection -- ^ Returns the finished 'Connection'
createConnection handle isClientSide = do
  -- Disable buffering
  liftIO $ hSetBuffering handle NoBuffering
  -- Create the actual connection structure
  let channels = [MkChannel 0 $ MkChannelType "im.ricochet.control-channel"]
      connection = MkConnection handle channels isClientSide B.empty
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
