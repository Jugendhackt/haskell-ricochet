{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.Ricochet.Protocol.Packets
import Network.Ricochet.Protocol.Protobuf (d, ext, msg)
import Network.Ricochet.Protocol.Protobuf.AuthHiddenService (accepted,
         client_cookie, is_known_contact, proof, public_key, result,
         server_cookie, signature)
import qualified Network.Ricochet.Protocol.Protobuf.AuthHiddenService as Auth
         (Packet)
import Network.Ricochet.Protocol.Protobuf.Chat (chat_acknowledge, chat_message,
         message_id)
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
         ConnectionRole(..), Packet, makePacket, pChannelID, pPacketData)

import Control.Concurrent (forkIO)
import Control.Lens ((&), (#), (.~), (^.), (^?), (^?!), _Just, filtered, re, strict)
import Control.Monad (void)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Network

tr c p = dupChannel c >>= \c' -> readChannel $ transformRO p c'

ourTorDomain = "m2chfjihyvrcmn4i"

main = do
  connection <- waitForConnection (PortNumber 5000)
  flip runStateT connection $ do
    v <- pickVersion [1]
    case v of
      Nothing -> error "We don’t have any version in common with the client"
      Just 1 -> do
        liftIO $ putStrLn "Version 1 chosen"
        chan <- get >>= liftIO . newPacketChannel
        liftIO . forkIO . void $ channelResult chan
        liftIO $ openChannel chan
        liftIO $ putStrLn "we should never get here"

channelResult :: Channel Packet Packet -> IO a
channelResult chan = forever $ do
  val <- tr chan $ selectChannel 0 . pPacketData . msg . channel_result . _Just
  case val ^. channel_identifier of
    2 -> do 
      putStrLn "Channel 2 opened!"

openChannel :: Channel Packet Packet -> IO a
openChannel chan = forever $ do
  val <- tr chan $ selectChannel 0 . pPacketData . msg . open_channel . _Just
  case val ^?! channel_type of
    MkChannelType "im.ricochet.auth.hidden-service" -> do
      void . forkIO $ authHiddenService chan val
    MkChannelType "im.ricochet.contact.request" -> do
      void . forkIO $ contactRequest chan val
    MkChannelType "im.ricochet.chat" -> do
      void . forkIO $ chat chan val
    MkChannelType c -> do
      putStrLn $ "Unknown channel type " <> show c

authHiddenService :: Channel Packet Packet -> OpenChannel -> IO ()
authHiddenService chan o = do
  case o ^? client_cookie of
    Nothing -> do
      putStrLn "No client cookie provided!"
    Just cookie -> do
      let r :: Control.Packet = d & channel_result .~ Just
               (d & channel_identifier .~ (o ^. channel_identifier)
                  & opened .~ True
                  & common_error .~ Nothing
                  & server_cookie .~ cookie) -- TODO: Generate a random cookie
      writeChannel chan . makePacket 0 $ msg # r
      p <- tr chan $ selectChannel (o ^. channel_identifier) . pPacketData . msg . proof . _Just
      case (p ^? public_key . publicDER, p ^? signature) of
        (Just publicKey, Just signature) -> do
          let theirTorDomain = torDomain $ publicKey
          let cookie' = cookie ^?! _Just . strict
          let prf = hmacSHA256 (cookie' <> cookie') (theirTorDomain <> ourTorDomain)
          case rawRSAVerify publicKey prf signature of
            True -> do
              putStrLn "Success!"
              let r :: Auth.Packet = d & result .~ Just
                       (d & accepted .~ True
                          & is_known_contact .~ Just False)
              writeChannel chan . makePacket (o ^. channel_identifier) $ msg # r
            False -> putStrLn "Failure!"
        _ -> putStrLn "Public key or signature missing!"

contactRequest :: Channel Packet Packet -> OpenChannel -> IO ()
contactRequest chan o = do
  case o ^? contact_request of
    Nothing -> putStrLn "No contact request provided!"
    Just req -> do
      print req
      let r :: Control.Packet = d & channel_result .~ Just
               (d & response .~ Just (d & status .~ Accepted)
                  & opened .~ True
                  & common_error .~ Nothing
                  & channel_identifier .~ (o ^. channel_identifier))
      writeChannel chan . makePacket 0 $ msg # r

chat :: Channel Packet Packet -> OpenChannel -> IO a
chat chan o = do
  let r :: Control.Packet = d & channel_result .~ Just
           (d & opened .~ True
              & common_error .~ Nothing
              & channel_identifier .~ (o ^. channel_identifier))
  writeChannel chan . makePacket 0 $ msg # r
  let o' :: Control.Packet = d & open_channel .~ Just
            (d & channel_identifier .~ 2
               & channel_type .~ MkChannelType "im.ricochet.chat")
  writeChannel chan . makePacket 0 $ msg # o'
  forever $ do
    val <- tr chan $ selectChannel (o ^. channel_identifier) . pPacketData . msg . chat_message . _Just
    let a :: Chat.Packet = d & chat_acknowledge .~ Just
             (d & message_id .~ val ^. message_id)
    writeChannel chan . makePacket (o ^. channel_identifier) $ msg # a
    print val

waitForConnection :: PortID -> IO Connection
waitForConnection port = do
  socket <- listenOn port
  (handle, hostname, portNumber) <- accept socket
  putStrLn $ "Client connected: " <> hostname <> ", port " <> show portNumber
  return $ MkConnection handle "" Server
