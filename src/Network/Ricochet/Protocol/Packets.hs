{-|
  Module:      Network.Ricochet.Protocol.Packets
  Description: Parsing, dumping and splitting low-level Packets

This module implements the parsing and dumping of the low-level Packets (ie.
those not described via Google Protobuf), as well as splitting a ByteString into
a series of Packets.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
module Network.Ricochet.Protocol.Packets
  ( parsePacket
  , dumpPacket
  , splitIntoPackets
  , packet
  , newPacketChannel
  , selectChannel
  ) where

import           Prelude                    hiding (take)

import           Network.Ricochet.Types     (Connection(..), Direction(..),
                                             Packet (..), ParserResult (..),
                                             _Success, cInputBuffer, makePacket,
                                             pChannelID, pDirection)
import           Network.Ricochet.Channel   (Channel, dupChannel, newChannel,
                                             readChannel, writeChannel)
import           Network.Ricochet.Util      (anyWord16, lookWith, parserResult)

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               (Prism', (^.), (^?), (.=), _1,
                                             filtered, prism')
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
  return $ MkPacket size channel packetData Received

-- | Serializes a Packet to yield it’s low-level representation
dumpPacket :: Packet -> ByteString
dumpPacket (MkPacket w1 w2 bs _) = toStrict . toLazyByteString $ word16BE w1 <> word16BE w2 <> byteString bs

-- | Prism for parsing and dumping packets.  This prism breaks the laws, since
--   parsePacket assumes the packet is 'Received'.
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

-- | Create a channel that lets you read and write Packets to the given
--   Connection
newPacketChannel :: Connection -> IO (Channel Packet Packet)
newPacketChannel c@(MkConnection h b _) = do
  chan <- newChannel
  forkIO . void $ runStateT (forever $ nextPacket >>= liftIO . writeChannel chan) c
  chanReadDuplicate <- dupChannel chan
  forkIO . forever $ readChannel chanReadDuplicate >>= sendPacket h
  return chan

-- | Checks if a complete packet is available on the given connection, and if
--   so, reads and returns it.
peekPacket :: StateT Connection IO (Maybe Packet)
peekPacket = do
  (MkConnection handle buffer _) <- get
  readBytes <- liftIO $ B.hGetNonBlocking handle maxPacketSize
  let buffer' = buffer <> readBytes
  cInputBuffer .= buffer'
  -- Try parsing a full packet and return it on success
  case parsePacket buffer' of
    Success packet rest -> do
      cInputBuffer .= rest
      return $ Just packet
    Unfinished -> return Nothing
    Failure    -> return Nothing
  where maxPacketSize = fromIntegral (maxBound :: Word16)

-- | Waits for a complete packet to arrive and returns it
nextPacket :: StateT Connection IO Packet
nextPacket = do
  maybePacket <- peekPacket
  case maybePacket of
    Just pkt -> return pkt
    Nothing -> liftIO (threadDelay delay) >> nextPacket
  where delay = round $ 10 ** 4

-- | Sends a Packet on the given Handle, unless it is 'Received'.
sendPacket :: Handle -> Packet -> IO ()
sendPacket handle pkt = case pkt ^. pDirection of
  Sent -> B.hPutStr handle $ dumpPacket pkt
  Received -> print pkt

selectChannel :: Word16 -> Prism' Packet Packet
selectChannel n = filtered ((== n) . (^. pChannelID))
