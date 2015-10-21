{-# LANGUAGE TupleSections #-}
module Network.Ricochet.Protocol.Lowest
  ( parsePacket
  , dumpPacket
  , splitInPackets
  ) where

import           Prelude                    hiding (take)

import           Network.Ricochet.Types     (Packet (..), makePacket)
import           Network.Ricochet.Util

import           Data.Attoparsec.ByteString
import           Data.Bifunctor             (first)
import           Data.ByteString            (ByteString ())
import qualified Data.ByteString            as B
import           Data.ByteString.Builder    (byteString, toLazyByteString,
                                             word16BE)
import           Data.ByteString.Lazy       (toStrict)
import           Data.Monoid                ((<>))
import           Data.Word                  (Word16, Word8)


-- | Actually parses a packet.
parsePacket :: ByteString -> Maybe (Maybe (Packet, ByteString))
parsePacket bs = maybeResult' . parse packet $ bs

-- | Parser for the low-level representation of a packet.
packet :: Parser Packet
packet = do
  size <- anyWord16
  channel <- anyWord16
  packetData <- take (fromIntegral size - 4)
  return $ MkPacket size channel packetData

-- | Serializes a Packet to yield it’s low-level representation
dumpPacket :: Packet -> ByteString
dumpPacket (MkPacket w1 w2 bs) = toStrict . toLazyByteString $ word16BE w1 <> word16BE w2 <> byteString bs

splitInPackets :: Word16 -> ByteString -> [Packet]
splitInPackets chan bs = let (ps, bs') = splitInPackets' chan ([], bs)
                         in ps <> [makePacket chan bs']

splitInPackets' :: Word16 -> ([Packet], ByteString) -> ([Packet], ByteString)
splitInPackets' chan (ps, bs) =
    case (toInteger . B.length $ bs) `compare` toInteger packLen of
      EQ -> (ps <> [makePacket chan bs], B.empty)
      LT -> (ps, bs)
      GT -> splitInPackets' chan (ps <> [makePacket chan (B.take packLen bs)], B.drop packLen bs)
  where packLen = fromIntegral (maxBound :: Word16) - 4
