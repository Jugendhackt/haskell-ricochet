{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Network.Ricochet.Crypto (hmacSHA256, publicDER, rawRSAVerify, torDomain)
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
import Data.Word (Word16)
import Data.Monoid
import Network
import Text.ProtocolBuffers (Wire, ReflectDescriptor)

ourTorDomain = "m2chfjihyvrcmn4i"

data Stuff = MkStuff { _chan :: Channel Packet Packet
                     , _chatChan :: MVar () }

makeLenses ''Stuff

main = do
  connection <- waitForConnection (PortNumber 5000)
  flip runStateT connection $ do
    v <- pickVersion [1]
    case v of
      Nothing -> error "We don’t have any version in common with the client"
      Just 1 -> do
        liftIO $ putStrLn "Version 1 chosen"
        c <- get >>= liftIO . newPacketChannel
        cc <- liftIO newEmptyMVar
        let stat = MkStuff c cc
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

openChannel :: ReaderT Stuff IO a
openChannel = forever $ do
  val <- tr $ selectChannel 0 . filtered ((== Received) . (^. pDirection)) . pPacketData . msg . open_channel . _Just
  case val ^?! channel_type of
    MkChannelType "im.ricochet.auth.hidden-service" -> do
      void . frk $ authHiddenService val
    MkChannelType "im.ricochet.contact.request" -> do
      void . frk $ contactRequest val
    MkChannelType "im.ricochet.chat" -> do
      void . frk $ chat val
    MkChannelType c -> do
      liftIO . putStrLn $ "Unknown channel type " <> show c

authHiddenService :: OpenChannel -> ReaderT Stuff IO ()
authHiddenService o = do
  case o ^? client_cookie of
    Nothing -> do
      liftIO $ putStrLn "No client cookie provided!"
    Just cookie -> do
      let r :: Control.Packet = d & channel_result .~ Just
               (d & channel_identifier .~ (o ^. channel_identifier)
                  & opened .~ True
                  & common_error .~ Nothing
                  & server_cookie .~ cookie) -- TODO: Generate a random cookie
      wr 0 r
      p <- tr $ selectChannel (o ^. channel_identifier) . pPacketData . msg . proof . _Just
      case (p ^? public_key . publicDER, p ^? signature) of
        (Just publicKey, Just signature) -> do
          let theirTorDomain = torDomain $ publicKey
          let cookie' = cookie ^?! _Just . strict
          let prf = hmacSHA256 (cookie' <> cookie') (theirTorDomain <> ourTorDomain)
          case rawRSAVerify publicKey prf signature of
            True -> do
              liftIO $ putStrLn "Success!"
              let r :: Auth.Packet = d & result .~ Just
                       (d & accepted .~ True
                          & is_known_contact .~ Just False)
              wr (o ^. channel_identifier) r
            False -> liftIO $ putStrLn "Failure!"
        _ -> liftIO $ putStrLn "Public key or signature missing!"

contactRequest :: OpenChannel -> ReaderT Stuff IO ()
contactRequest o = do
  case o ^? contact_request of
    Nothing -> liftIO $ putStrLn "No contact request provided!"
    Just req -> do
      liftIO $ print req
      let r :: Control.Packet = d & channel_result .~ Just
               (d & response .~ Just (d & status .~ Accepted)
                  & opened .~ True
                  & common_error .~ Nothing
                  & channel_identifier .~ (o ^. channel_identifier))
      wr 0 r

chat :: OpenChannel -> ReaderT Stuff IO a
chat o = do
  let r :: Control.Packet = d & channel_result .~ Just
           (d & opened .~ True
              & common_error .~ Nothing
              & channel_identifier .~ (o ^. channel_identifier))
  wr 0 r
  let o' :: Control.Packet = d & open_channel .~ Just
            (d & channel_identifier .~ 2
               & channel_type .~ MkChannelType "im.ricochet.chat")
  wr 0 o'
  view chatChan >>= liftIO . takeMVar
  forever $ do
    val <- tr $ selectChannel (o ^. channel_identifier) . pPacketData . msg . chat_message . _Just
    let a :: Chat.Packet = d & chat_acknowledge .~ Just
             (d & message_id .~ val ^. message_id)
    wr (o ^. channel_identifier) a
    liftIO $ print val
    let m :: Chat.Packet = d & chat_message .~ Just
             (d & message_text .~ (val ^. message_text . reversed))
    wr 2 m

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
