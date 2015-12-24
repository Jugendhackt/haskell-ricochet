{-|
  Module: Network.Ricochet.Version
  Description: Implementation of the parser and dumper for 'Version's

"Network.Ricochet.Version" contains the implementations of the parser and dumper
for the version negotiation step of the protocol, as well as some related type
definitions.
-}

{-# LANGUAGE OverloadedStrings #-}

module Network.Ricochet.Protocol.Version
  ( Version ()
  , parseIntroduction
  , dumpIntroduction
  , offerVersions
  , pickVersion
  ) where

import           Prelude                    hiding (lookup)

import           Network.Ricochet.Util      (parserResult)
import           Network.Ricochet.Types     (Connection (..), ParserResult(..),
                                             cHandle, cInputBuffer)

import           Control.Applicative        ((<|>))
import           Control.Concurrent         (threadDelay)
import           Control.Lens               ((<%=), (.=), use)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State        (MonadState (..), StateT (..))
import           Data.Attoparsec.ByteString (Parser, anyWord8, count, parse,
                                             string)
import           Data.ByteString            (ByteString ())
import qualified Data.ByteString            as B
import           Data.List                  (elem, intersect)
import           Data.Map                   (Map (), filterWithKey, keys,
                                             lookup, size)
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))
import           GHC.Word                   (Word8 ())
import           System.IO                  (Handle ())

-- | A Version is a Word8 as defined by the ricochet protocol
type Version = Word8

-- | Parses Introduction and Version Negotiation of the protocol
parseIntroduction :: ByteString -> ParserResult [Version]
parseIntroduction = parserResult . parse (introductionParser)

-- | Creates a Parser for the introduction step of the protocol
introductionParser :: Parser [Version]
introductionParser = do
  string "IM"
  nVersions <- anyWord8
  count (fromIntegral nVersions) anyWord8

-- | Dumps the introduction message
dumpIntroduction :: [Version] -> ByteString
dumpIntroduction vers = "IM" <> versionCount <> versions
  where versionCount = B.singleton (fromIntegral . length $ vers :: Word8)
        versions     = B.concat $ map B.singleton vers

-- | Offers the available versions to the newly connected peer and returns the
--   version the peer picked
offerVersions :: [Version] -> StateT Connection IO (Maybe Version)
offerVersions vers = do
  handle <- use cHandle
  -- Send the available versions
  liftIO . B.hPutStr handle $ dumpIntroduction vers
  response <- liftIO $ B.hGet handle 1
  let choice = head $ B.unpack response
  -- If the choice is valid
  if choice `elem` vers
    then return (Just choice)
    else return Nothing

-- | Picks a version supported by the peer, sends him the choice and returns it
pickVersion :: [Version] -> StateT Connection IO (Maybe Version)
pickVersion vers = do
  handle <- use cHandle
  response <- awaitVersionsOffer vers
  case response of
    -- No matching versions
    Just [] -> do
      liftIO . B.hPutStr handle $ B.singleton 0xFF
      return Nothing
    -- The versions match
    Just matching -> do
      -- Always choose the latest version
      let chosen = maximum matching
      liftIO . B.hPutStr handle $ B.singleton chosen
      return $ Just chosen
    Nothing -> return Nothing

-- | Waits for the peer to send the introductory message, and returns it in
--   parsed form
awaitVersionsOffer :: [Version] -> StateT Connection IO (Maybe [Version])
awaitVersionsOffer vers = do
  handle <- use cHandle
  input <- liftIO $ B.hGetNonBlocking handle 300
  introMessage <- cInputBuffer <%= (<> input)
  case intersect vers <$> parseIntroduction introMessage of
   Success vers rest -> do
     cInputBuffer .= rest
     return $ Just vers
   Unfinished    -> liftIO (threadDelay delay) >> awaitVersionsOffer vers
   Failure       -> return Nothing
  where delay = round $ 10 ** 4
