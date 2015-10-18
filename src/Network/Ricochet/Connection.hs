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
    -- TODO: This is not elegant, use pattern guards
    case fmap (first M.toList) maybeStuff of
      Just ([], rest) -> do
        liftIO $ putStrLn "We don’t have any versions in common with remote side"
        liftIO . B.putStr $ B.pack [0xFF]
      Just (handlers, rest) -> do
        let chosen = foldl1 max (fmap fst handlers)
        liftIO . putStrLn $ "We have chosen " <> show chosen <> "."
        liftIO . putStrLn $ "We can choose between " <> show (length handlers) <> " versions!"
        liftIO . B.putStr $ B.pack [chosen]
        (fromJust $ lookup chosen handlers) con
      Nothing -> liftIO $ putStrLn "Remote side sent invalid version negotiation."
  return con

awaitIntroMessage :: Versions -> Handle -> IO (Maybe (Versions, ByteString))
awaitIntroMessage vers handle = do
  introMessage <- B.hGetNonBlocking handle maxBound
  case parseIntroduction vers introMessage of
   Just (Just map) -> return $ Just map
   Just Nothing -> liftIO (threadDelay delay) >> awaitIntroMessage vers handle
   Nothing -> return Nothing
  where delay = round $ 10 ** 4
