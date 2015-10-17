{-# LANGUAGE TupleSections #-}
module Network.Ricochet.Protocol.Lowest
  (parsePacket, dumpPacket, parseWord16) where

import           Data.Bifunctor         (first)
import           Data.Bits              (shiftL)
import qualified Data.ByteString        as B
import           Data.ByteString        (ByteString())
import           Data.ByteString.Builder(word16BE, byteString, toLazyByteString)
import           Data.ByteString.Lazy   (toStrict)
import           Data.Monoid            ((<>))
import           Data.Word              (Word8, Word16)
import           Network.Ricochet.Types (Packet (..))

data Sometimes a b = NotYet (a -> Sometimes a b)
                   | ThereYouGo b

instance Show b => Show (Sometimes a b) where
  show (NotYet _) = "NotYet"
  show (ThereYouGo b) = "ThereYouGo" <> show b

-- | Parses the low-level representation of a Packet.
parsePacket :: B.ByteString -> (Sometimes B.ByteString (Packet, B.ByteString))
parsePacket bs = case parseWord16 bs of
    Just (size, bs') -> case parseWord16 bs' of
      Just (channelId, bs'') -> case (toInteger . B.length $ bs'') `compare` toInteger size of
        EQ -> ThereYouGo . (,B.empty) $ MkPacket size channelId bs''
        GT -> ThereYouGo $ (MkPacket size channelId $ B.take (conv size) bs'', B.drop (conv size) bs'')
        LT -> NotYet $ parsePacket . (<> bs'')
      Nothing -> NotYet $ parsePacket . (<> bs')
    Nothing -> NotYet $ parsePacket . (<> bs)
  where conv = fromInteger . toInteger

-- | Serializes a Packet to yield it’s low-level representation
dumpPacket :: Packet -> B.ByteString
dumpPacket (MkPacket w1 w2 bs) = toStrict . toLazyByteString $ word16BE w1 <> word16BE w2 <> byteString bs

-- | Parses two Word8s from a ByteString into one Word16
parseWord16 :: B.ByteString -> Maybe (Word16, B.ByteString)
parseWord16 bs = do
    if B.length bs < 2
        then Nothing
        else
            let (words, rest) = B.splitAt 2 bs
            in Just . (,rest) . joinWords . B.unpack $ words
    where
        joinWords [a, b] = (toWord16 a `shiftL` 8) + toWord16 b

        toWord16 :: Word8 -> Word16
        toWord16 = fromIntegral
