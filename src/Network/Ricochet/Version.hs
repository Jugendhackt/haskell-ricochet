{-# LANGUAGE OverloadedStrings #-}

module Network.Ricochet.Version
  ( Versions ()
  , Version ()
  , ConnectionHandler ()
  , parseIntroduction
  , dumpIntroduction
  ) where

import           Prelude                    hiding (lookup)

import           Network.Ricochet.Monad
import           Network.Ricochet.Util
import           Network.Ricochet.Types

import           Control.Applicative        ((<|>))
import           Data.Attoparsec.ByteString
import           Data.ByteString            (ByteString ())
import qualified Data.ByteString            as B
import           Data.List                  (elem)
import           Data.Map                   (Map (), filterWithKey, keys,
                                             lookup, size)
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))
import           GHC.Word                   (Word8 ())

-- | A Version is a Word8 as defined by the ricochet protocol
type Version = Word8

-- | Handles a connection between two users
type ConnectionHandler = Connection -> Ricochet ()

-- | Each version (Word8) has its own Handler that takes a Connection which has
-- completed version negotiation and does Ricochet actions
type Versions = Map Version ConnectionHandler

-- | Parses Introduction and Version Negotiation of the protocol
parseIntroduction :: Versions -> ByteString -> ParserResult Versions
parseIntroduction vers bs = parserResult . parse (introductionParser vers) $ bs

introductionParser :: Map Version ConnectionHandler -> Parser (Map Version ConnectionHandler)
introductionParser supportedVersions = do
  string "IM"
  nVersions <- anyWord8
  versions <- count (fromIntegral nVersions) anyWord8
  return $ filterWithKey (\k _ -> k `elem` versions) supportedVersions

dumpIntroduction :: Versions -> ByteString
dumpIntroduction supportedVersions = "IM" <> B.singleton (fromIntegral . size $ supportedVersions :: Word8) <>
  foldl (\s c -> s <> B.singleton c) "" (keys supportedVersions)
