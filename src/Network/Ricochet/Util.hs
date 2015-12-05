{-|
  Module: Network.Ricochet.Util
  Description: A collection of utility functions

"Network.Ricochet.Util" contains a collection of utility functions used
throughout the package.
-}

{-# LANGUAGE RankNTypes #-}

module Network.Ricochet.Util
  ( parserResult
  , anyWord16
  , joinWord8s
  , look
  , lookWith
  ) where

import           Network.Ricochet.Types

import           Control.Lens
import           Control.Monad                    (liftM2)
import           Data.Attoparsec.ByteString
import           Data.Bits                        (shiftL)
import           Data.ByteString                  (ByteString ())
import           GHC.Word

-- | Joins two Word8s into a single Word16
joinWord8s :: Word8 -> Word8 -> Word16
joinWord8s a b = (fromIntegral a `shiftL` 8) + fromIntegral b

-- | Parses two bytes into a Word16
anyWord16 :: Parser Word16
anyWord16 = liftM2 joinWord8s anyWord8 anyWord8

-- | Takes an attoparsec result and converts it
--   into a our representation.
parserResult :: Result r -> ParserResult r
parserResult (Done i r)  = Success r i
parserResult (Partial _) = Unfinished
parserResult _           = Failure

-- | Lookup an item in a list
look :: (Eq a) => a -> Traversal' [a] a
look x = traversed . filtered (== x)

-- | Lookup an item in a list, extracting an identifier using the given
--   Traversal before comparison
lookWith :: (Eq b) => Traversal' a b -> b -> Traversal' [a] a
lookWith t x = traversed . filtered ((== Just x) . (^? t))
