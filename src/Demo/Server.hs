{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Network.Ricochet.Protocol.Packets
import Network.Ricochet.Protocol.Protobuf (d, ext, msg)
import Network.Ricochet.Protocol.Protobuf.AuthHiddenService (accepted,
         client_cookie, is_known_contact, proof, public_key, result,
         server_cookie, signature)
import qualified Network.Ricochet.Protocol.Protobuf.AuthHiddenService as Auth
         (Packet)
import Network.Ricochet.Protocol.Protobuf.Chat (chat_acknowledge, chat_message,
         message_id, message_text)
import qualified Network.Ricochet.Protocol.Protobuf.Chat as Chat (Packet)
import Network.Ricochet.Protocol.Protobuf.ContactRequest (ContactRequest,
         Response, Status(..), contact_request, response, status)
import Network.Ricochet.Protocol.Protobuf.ControlChannel (OpenChannel,
         open_channel, opened, channel_result, channel_type, channel_identifier,
         common_error)
import qualified Network.Ricochet.Protocol.Protobuf.ControlChannel as Control
         (Packet)
import Network.Ricochet.Protocol.Version
import Network.Ricochet.Channel (Channel, dupChannel, readChannel, transformRO,
         writeChannel)
import Network.Ricochet.Crypto (base64, generate1024BitRSA, hmacSHA256,
         privateDER, publicDER, rawRSAVerify, torDomain)
import Network.Ricochet.Types (ChannelType(..), Connection(..),
         ConnectionRole(..), Direction(..), Packet, makePacket, pChannelID,
         pDirection, pPacketData)

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, takeMVar, newEmptyMVar, putMVar)
import Control.Lens (ATraversal', (&), (#), (.~), (^.), (^?), (^?!), _Just,
         filtered, re, reversed, strict, makeLenses, use, view)
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.Base32String.Default (toText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (putStrLn)
import Data.Text (toLower)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16)
import Data.Monoid
import Network
import Network.Anonymous.Tor (withSession, mapOnion)
import OpenSSL.RSA (RSAKeyPair)
import Text.ProtocolBuffers (Wire, ReflectDescriptor)

data Stuff = MkStuff { _chan :: Channel Packet Packet
                     , _chatChan :: MVar ()
                     , _ourDomain :: ByteString
                     , _ourKey :: RSAKeyPair }

makeLenses ''Stuff

main = do
  key <- generate1024BitRSA
  let encodedKey = base64 . privateDER # key
  void . withSession 9051 $ \ctrlSock -> do
    address <- encodeUtf8 . toLower . toText <$> mapOnion ctrlSock 9878 5000 False (Just encodedKey)
    B.putStrLn $ "Our hidden domain address is: " <> address
    connection <- waitForConnection (PortNumber 5000)
    flip runStateT connection $ do
      v <- pickVersion [1]
      case v of
        Nothing -> error "We don’t have any version in common with the client"
        Just 1 -> do
          liftIO $ putStrLn "Version 1 chosen"
          c <- get >>= liftIO . newPacketChannel
          cc <- liftIO newEmptyMVar
          let stat = MkStuff c cc address key
          liftIO . forkIO . void $ runReaderT channelResult stat
          liftIO $ runReaderT openChannel stat
          liftIO $ putStrLn "we should never get here"

channelResult :: ReaderT Stuff IO a
channelResult = forever $ do
  val <- tr $ selectChannel 0 . pPacketData . msg . channel_result . _Just
  case val ^. channel_identifier of
    2 -> do
      liftIO $ putStrLn "Channel 2 opened!"
      cc <- view chatChan
      liftIO $ putMVar cc ()
    _ -> return ()

openChannel :: ReaderT Stuff IO ()
openChannel = void $ do
  frk $ openChannelHandler (MkChannelType "im.ricochet.auth.hidden-service") authHiddenService
  frk $ openChannelHandler (MkChannelType "im.ricochet.contact.request") contactRequest
  openChannelHandler (MkChannelType "im.ricochet.chat") chat

openChannelHandler :: ChannelType -> (OpenChannel -> ReaderT Stuff IO ()) -> ReaderT Stuff IO a
openChannelHandler t f = forever $ do
  val <- tr $ selectChannel 0 . filtered ((== Received) . (^. pDirection)) .
       pPacketData . msg . open_channel . _Just .
       filtered ((== Just t) . (^? channel_type))
  void . frk $ f val

authHiddenService :: OpenChannel -> ReaderT Stuff IO ()
authHiddenService o = do
  case o ^? client_cookie of
    Nothing -> do
      liftIO $ putStrLn "No client cookie provided!"
    Just cookie -> do
      wr 0 $ d & channel_result .~ Just
                 (d & channel_identifier .~ (o ^. channel_identifier)
                    & opened .~ True
                    & common_error .~ Nothing
                    & server_cookie .~ cookie) -- TODO: Generate a random cookie
      p <- tr $ selectChannel (o ^. channel_identifier) . pPacketData . msg . proof . _Just
      case (p ^? public_key . publicDER, p ^? signature) of
        (Just publicKey, Just signature) -> do
          let theirTorDomain = torDomain $ publicKey
          let cookie' = cookie ^?! _Just . strict
          ourDom <- view ourDomain
          let prf = hmacSHA256 (cookie' <> cookie') (theirTorDomain <> ourDom)
          case rawRSAVerify publicKey prf signature of
            True -> do
              liftIO $ putStrLn "Success!"
              wr (o ^. channel_identifier) $ d & result .~ Just
                                                 (d & accepted .~ True
                                                    & is_known_contact .~ Just False)
            False -> liftIO $ putStrLn "Failure!"
        _ -> liftIO $ putStrLn "Public key or signature missing!"

contactRequest :: OpenChannel -> ReaderT Stuff IO ()
contactRequest o = do
  case o ^? contact_request of
    Nothing -> liftIO $ putStrLn "No contact request provided!"
    Just req -> do
      liftIO $ print req
      wr 0 $ d & channel_result .~ Just
                 (d & response .~ Just (d & status .~ Accepted)
                    & opened .~ True
                    & common_error .~ Nothing
                    & channel_identifier .~ (o ^. channel_identifier))

chat :: OpenChannel -> ReaderT Stuff IO a
chat o = do
  wr 0 $ d & channel_result .~ Just
             (d & opened .~ True
                & common_error .~ Nothing
                & channel_identifier .~ (o ^. channel_identifier))
  wr 0 $ d & open_channel .~ Just
             (d & channel_identifier .~ 2
                & channel_type .~ MkChannelType "im.ricochet.chat")
  view chatChan >>= liftIO . takeMVar
  forever $ do
    val <- tr $ selectChannel (o ^. channel_identifier) . pPacketData . msg . chat_message . _Just
    wr (o ^. channel_identifier) $ d & chat_acknowledge .~ Just
                                       (d & message_id .~ val ^. message_id)
    liftIO $ print val
    wr 2 $ d & chat_message .~ Just
               (d & message_text .~ (val ^. message_text . reversed))

waitForConnection :: PortID -> IO Connection
waitForConnection port = do
  socket <- listenOn port
  (handle, hostname, portNumber) <- accept socket
  putStrLn $ "Client connected: " <> hostname <> ", port " <> show portNumber
  return $ MkConnection handle "" Server

tr :: ATraversal' Packet a -> ReaderT Stuff IO a
tr p = view chan >>= liftIO . dupChannel >>= liftIO . readChannel . transformRO p

wr :: (ReflectDescriptor msg, Wire msg) => Word16 -> msg -> ReaderT Stuff IO ()
wr i m = view chan >>= \c -> liftIO . writeChannel c . makePacket i $ msg # m

frk :: ReaderT r IO () -> ReaderT r IO ThreadId
frk x = ask >>= liftIO . forkIO . runReaderT x
