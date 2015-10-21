module Network.Ricochet.Util
  ( maybeResult'
  , anyWord16
  , joinWord8s
  ) where

import           Control.Monad              (liftM2)
import           Data.Attoparsec.ByteString
import           Data.Bits                  (shiftL)
import           Data.ByteString            (ByteString ())
import           GHC.Word

-- | Joins two Word8s into a single Word16
joinWord8s :: Word8 -> Word8 -> Word16
joinWord8s a b = (fromIntegral a `shiftL` 8) + fromIntegral b

-- | Takes an attoparsec result and converts it
-- into a representation that is useful for our
-- library's internals
maybeResult' :: Result r -> Maybe (Maybe (r, ByteString))
maybeResult' (Done i r) = Just $ Just (r, i)
maybeResult' (Partial _) = Just Nothing
maybeResult' _          = Nothing

-- | Parses two bytes into a Word16
anyWord16 :: Parser Word16
anyWord16 = liftM2 joinWord8s anyWord8 anyWord8
