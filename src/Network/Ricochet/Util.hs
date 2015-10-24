{-|
  Module: Network.Ricochet.Util
  Description: A collection of utility functions

"Network.Ricochet.Util" contains a collection of utility functions used
throughout the package.
-}

module Network.Ricochet.Util
  ( parserResult
  , anyWord16
  , joinWord8s
  ) where

import           Network.Ricochet.Types

import           Control.Monad              (liftM2)
import           Data.Attoparsec.ByteString
import           Data.Bits                  (shiftL)
import           Data.ByteString            (ByteString ())
import           GHC.Word

-- | Joins two Word8s into a single Word16
joinWord8s :: Word8 -> Word8 -> Word16
joinWord8s a b = (fromIntegral a `shiftL` 8) + fromIntegral b

-- | Takes an attoparsec result and converts it
-- into a our representation.
parserResult :: Result r -> ParserResult r
parserResult (Done i r)  = Success r i
parserResult (Partial _) = Unfinished
parserResult _           = Failure

-- | Parses two bytes into a Word16
anyWord16 :: Parser Word16
anyWord16 = liftM2 joinWord8s anyWord8 anyWord8
