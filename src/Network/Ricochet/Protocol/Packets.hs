{-|
  Module:      Network.Ricochet.Protocol.Packets
  Description: Parsing, dumping and splitting low-level Packets

This module implements the parsing and dumping of the low-level Packets (ie.
those not described via Google Protobuf), as well as splitting a ByteString into
a series of Packets.
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Ricochet.Protocol.Packets
  ( parsePacket
  , dumpPacket
  , splitIntoPackets
  , packet
  , newPacketChannel
  ) where

import           Prelude                    hiding (take)

import           Network.Ricochet.Types     (Packet (..), ParserResult (..),
                                             _Success, makePacket)
import           Network.Ricochet.Channel   (Channel, newChannel, readChannel,
                                             writeChannel)
import           Network.Ricochet.Util      (anyWord16, lookWith, parserResult)

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               (Prism', (^?), _1, prism')
import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State        (StateT (..), get, put, runStateT)
import           Data.Attoparsec.ByteString (Parser, parse, take)
import           Data.Bifunctor             (first)
import           Data.ByteString            (ByteString ())
import qualified Data.ByteString            as B
import           Data.ByteString.Builder    (byteString, toLazyByteString,
                                             word16BE)
import           Data.ByteString.Lazy       (toStrict)
import           Data.Monoid                ((<>))
import           Data.Word                  (Word16, Word8)
import           System.IO                  (Handle)


-- | Actually parses a packet.
parsePacket :: ByteString -> ParserResult Packet
parsePacket bs = parserResult . parse packetParser $ bs

-- | Parser for the low-level representation of a packet.
packetParser :: Parser Packet
packetParser = do
  size <- anyWord16
  channel <- anyWord16
  packetData <- take (fromIntegral size - 4)
  return $ MkPacket size channel packetData

-- | Serializes a Packet to yield it’s low-level representation
dumpPacket :: Packet -> ByteString
dumpPacket (MkPacket w1 w2 bs) = toStrict . toLazyByteString $ word16BE w1 <> word16BE w2 <> byteString bs

-- | Prism for parsing and dumping a packet
packet :: Prism' ByteString Packet
packet = prism' dumpPacket ((^? _Success . _1) . parsePacket)

-- | Split a ByteString into as many packets as necessary
splitIntoPackets :: Word16      -- ^ ID of the channel the packets should be sent on
                  -> ByteString -- ^ The ByteString to be split
                  -> [Packet]   -- ^ Returns a list of packets containing the ByteString
splitIntoPackets chan bs = let (ps, bs') = splitIntoPackets' chan ([], bs)
                           in  ps <> [makePacket chan bs']

-- | Iteratively turn a ByteString into a series of maximal sized packets and
--   return the rest
splitIntoPackets' :: Word16                 -- ^ ID of the channel the packets should be sent on
                  -> ([Packet], ByteString) -- ^ A tuple of the packets created so far and the
                                            --   ByteString to consume
                  -> ([Packet], ByteString) -- ^ Returns a tuple of the packets created and a
                                            --   ByteString shorter than the maximal packet length
splitIntoPackets' chan (ps, bs) =
    case (toInteger . B.length $ bs) `compare` toInteger maxPackLen of
      EQ -> (ps <> [makePacket chan bs], B.empty)
      LT -> (ps, bs)
      -- Call the function again if there is some ByteString left
      GT -> splitIntoPackets' chan (ps <> [makePacket chan (B.take maxPackLen bs)], B.drop maxPackLen bs)
  where maxPackLen = fromIntegral (maxBound :: Word16) - 4

-- | Create a channel that lets you read and write Packets to the given Handle
newPacketChannel :: (Handle, ByteString) -> IO (Channel Packet Packet)
newPacketChannel p@(h, _) = do
  c <- newChannel
  forkIO . void $ runStateT (forever $ nextPacket >>= liftIO . writeChannel c) p
  forkIO . forever $ readChannel c >>= sendPacket h
  return c

-- | Checks if a complete packet is available on the given connection, and if
--   so, reads and returns it.
peekPacket :: StateT (Handle, ByteString) IO (Maybe Packet)
peekPacket = do
  (handle, buffer) <- get
  readBytes <- liftIO $ B.hGetNonBlocking handle maxPacketSize
  let buffer' = buffer <> readBytes
  put (handle, buffer')
  -- Try parsing a full packet and return it on success
  case parsePacket buffer' of
    Success packet rest -> do
      put (handle, rest)
      return $ Just packet
    Unfinished -> return Nothing
    Failure    -> return Nothing
  where maxPacketSize = fromIntegral (maxBound :: Word16)

-- | Waits for a complete packet to arrive and returns it
nextPacket :: StateT (Handle, ByteString) IO Packet
nextPacket = do
  maybePacket <- peekPacket
  case maybePacket of
    Just pkt -> return pkt
    Nothing -> liftIO (threadDelay delay) >> nextPacket
  where delay = round $ 10 ** 6

-- | Sends a Packet to a connected User
sendPacket :: Handle -> Packet -> IO ()
sendPacket handle pkt = B.hPutStr handle $ dumpPacket pkt
