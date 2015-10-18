{-# LANGUAGE OverloadedStrings #-}

module Network.Ricochet.Connection
  ( createState
  , awaitConnection
  , connectTo
  ) where

import           Network.Ricochet.Monad
import           Network.Ricochet.Types
import           Network.Ricochet.Version

import           Control.Concurrent     (threadDelay)
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString())
import qualified Data.ByteString        as B
import qualified Data.Map               as M
import           Data.Monoid            ((<>))
import           Network                (PortID (..), accept, listenOn)
import           Network.Socks5         (socksConnectTo)
import           System.IO              (Handle (), hSetBuffering, BufferMode(..))

createState :: PortID -> IO RicochetState
createState port = do
  sock <- listenOn port
  return $ MkRicochetState sock [] [] (PortNumber 9050) defaultVersions

defaultVersions :: Versions
defaultVersions = M.fromList
  [ ( 0x01
    , \con -> return ())
  ]

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
  liftIO $ hSetBuffering handle NoBuffering
  let con = MkConnection handle [MkChannel 0 $ MkChannelType "im.ricochet.control-channel"] isClientSide B.empty
  connections %= (<> [con])
  vers <- use versions
  if isClientSide then do
    liftIO . B.hPutStr handle $ dumpIntroduction vers
    resp <- liftIO $ B.hGet handle 1
    let choice = head $ B.unpack resp
    if choice `M.member` vers then
      vers M.! choice $ con
    else liftIO $ putStrLn "Server responded with invalid protocol choice"
  else do
    maybeStuff <- liftIO $ awaitIntroMessage vers handle
    case maybeStuff of
      Just (x:xs, rest) -> do
        --let chosen = foldl max (M.keys handlers)
        --liftIO . putStrLn $ "We have chosen " <> show chosen <> "."
        liftIO . putStrLn $ "We can choose between " <> show (length xs + 1) <> " versions!"
        x con
      Just ([], rest) -> do
        liftIO $ putStrLn "Remote side sent no versions"
      Nothing -> liftIO $ putStrLn "Remote side sent invalid version negotiation."
  return con

awaitIntroMessage :: Versions -> Handle -> IO (Maybe ([ConnectionHandler], ByteString))
awaitIntroMessage vers handle = do
  introMessage <- B.hGetNonBlocking handle maxBound
  case parseIntroduction vers introMessage of
   Just (Just pair) -> return $ Just pair
   Just Nothing -> liftIO (threadDelay delay) >> awaitIntroMessage vers handle
   Nothing -> return Nothing
  where delay = round $ 10 ** 4
