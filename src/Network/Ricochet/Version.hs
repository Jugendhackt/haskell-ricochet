{-|
  Module: Network.Ricochet.Version
  Description: Implementation of the parser and dumper for 'Version's

"Network.Ricochet.Version" contains the implementations of the parser and dumper
for the version negotiation step of the protocol, as well as some related type
definitions.
-}

{-# LANGUAGE OverloadedStrings #-}

module Network.Ricochet.Version
  ( Versions ()
  , Version ()
  , ConnectionHandler ()
  , parseIntroduction
  , dumpIntroduction
  ) where

import           Prelude                    hiding (lookup)

import           Network.Ricochet.Util      (parserResult)
import           Network.Ricochet.Monad     (Ricochet)
import           Network.Ricochet.Types     (Connection, ParserResult)

import           Control.Applicative        ((<|>))
import           Data.Attoparsec.ByteString (Parser, anyWord8, count, parse,
                                             string)
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

-- | Handles a connection between two peers
type ConnectionHandler = Connection -> Ricochet ()

-- | Each version (Word8) has its own Handler that takes a Connection which has
--   completed version negotiation and does Ricochet actions
type Versions = Map Version ConnectionHandler

-- | Parses Introduction and Version Negotiation of the protocol
parseIntroduction :: Versions -> ByteString -> ParserResult Versions
parseIntroduction vers bs = parserResult . parse (introductionParser vers) $ bs

-- | Creates a Parser for the introduction step of the protocol
introductionParser :: Versions -> Parser (Map Version ConnectionHandler)
introductionParser supportedVersions = do
  string "IM"
  nVersions <- anyWord8
  versions <- count (fromIntegral nVersions) anyWord8
  -- Only return versions supported by this side of the connection
  return $ filterWithKey (\k _ -> k `elem` versions) supportedVersions

-- | Dumps the introduction message
dumpIntroduction :: Versions -> ByteString
dumpIntroduction supportedVersions = "IM" <> versionCount <> versions
  where versionCount = B.singleton (fromIntegral . size $ supportedVersions :: Word8) 
        versions     = foldl (\s c -> s <> B.singleton c) "" (keys supportedVersions)
