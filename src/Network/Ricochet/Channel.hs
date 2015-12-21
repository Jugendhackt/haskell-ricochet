{-|
  Module:   Network.Ricochet.Channel
  Description: Our custom spin on 'Channel's usually used in concurrent
               programming

"Network.Ricochet.Channel" implements our custom Channel type, which is the
base of most of the higher-level API.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.Ricochet.Channel
  ( Channel
  , newChannel
  , dupChannel
  , readChannel
  , writeChannel
  , transform
  , mergeChannel
  ) where

import Control.Concurrent.Chan
import Control.Lens hiding (transform)
import Control.Monad (liftM, (>=>))
import Control.Applicative (Alternative(..))

-- | A 'Channel' of the source type s, transformed to yield values of type a
data Channel s a = MkChannel (Chan s) (APrism' s a)

-- | Helper function for creating source channels
mkSource :: Chan s -> Channel s s
mkSource = flip MkChannel (prism' id Just)

-- | Create a new, empty 'Channel'
newChannel :: IO (Channel s s)
newChannel = mkSource <$> newChan

-- | Duplicate a 'Channel', yielding a 'Channel' transporting the source type.
dupSource :: Channel s a -> IO (Channel s s)
dupSource (MkChannel c _) = mkSource <$> dupChan c

-- | Duplicate a 'Channel'. Everything written on either on can be read on both
--   of them from now on.
dupChannel :: Channel s a -> IO (Channel s a)
dupChannel (MkChannel chan p) = flip MkChannel p <$> dupChan chan

-- | Read the next value from a 'Channel'
readChannel :: Channel s a -> IO a
readChannel c@(MkChannel chan p) = (^? clonePrism p) <$> readChan chan >>= \case
                        Just v  -> return v
                        Nothing -> readChannel c

-- | Write a value to a 'Channel'
writeChannel :: Channel s a -> a -> IO ()
writeChannel (MkChannel chan p) = writeChan chan . (clonePrism p #)

-- | Transform a 'Channel' into one yielding another type. This function can
--   also be used to filter over 'Channel's.
transform :: APrism' a b -> Channel s a -> Channel s b
transform q (MkChannel chan p) = MkChannel chan (clonePrism p.clonePrism q)

-- | Merge two 'Channel's with the same origin into one.
--
--   ATTENTION: Don't merge channels with different sources.
mergeChannel :: Channel s a -> Channel s b -> Channel s (Either a b)
mergeChannel (MkChannel chan p) (MkChannel chan' q)
  | chan /= chan' = error "Trying to merge two Channels with different sources"
  | otherwise     = MkChannel chan r
  where r = mergePrisms p q

-- | Merges two 'Prism's the way we need it for 'mergeChannel'
mergePrisms :: APrism' s a -> APrism' s b -> APrism' s (Either a b)
mergePrisms p q = prism' fromEither toEither
  where fromEither = either (clonePrism p #) (clonePrism q #)
        toEither s = Left <$> s ^? clonePrism p <|> Right <$> s ^? clonePrism q
