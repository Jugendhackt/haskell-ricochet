{-|
  Module:      Network.Ricochet.Protocol.Lowest
  Description: Implementation of the low-level protocol parsing

"Network.Ricochet.Protocol.Lowest" implements the parsing of the parts of the
protocol which aren't described via Google Protobuf.
-}

{-# LANGUAGE TupleSections #-}
module Network.Ricochet.Protocol.Lowest
  ( parsePacket
  , dumpPacket
  , splitIntoPackets
  , packet
  ) where

import Network.Ricochet.Util (anyWord16, parserResult)
import Network.Ricochet.Types (Packet (..), ParserResult (..), makePacket,
                                _Success)

import Prelude hiding (take)
import Control.Lens (Prism', (^?), _1, prism')
import Data.Attoparsec.ByteString (Parser, parse, take)
import Data.Bifunctor (first)
import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString, toLazyByteString, word16BE)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import Data.Word (Word16, Word8)


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
dumpPacket (MkPacket w1 w2 bs) = toStrict . toLazyByteString $ word16BE w1
                                   <> word16BE w2 <> byteString bs

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
