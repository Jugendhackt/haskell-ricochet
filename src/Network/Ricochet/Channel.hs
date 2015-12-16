{-|
  Module:   Network.Ricochet.Channel
  Description: Our custom spin on 'Channel's usually used in concurrent
               programming

"Network.Ricochet.Channel" implements our custom Channel type, which is the
base of most of the higher-level API.
-}

{-# LANGUAGE LambdaCase #-}

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
import Control.Monad (liftM, (>=>))
import Control.Applicative (Alternative(..))

-- | A 'Channel' of the source type s, transformed to yield values of type a
data Channel s a = MkChannel (Chan s) (s -> Maybe a)

-- | Create a new, empty 'Channel'
newChannel :: IO (Channel s s)
newChannel = flip MkChannel Just <$> newChan

-- | Duplicate a 'Channel'. Everything written on either on can be read on both
--   of them from now on.
dupChannel :: Channel s a -> IO (Channel s a)
dupChannel (MkChannel chan f) = flip MkChannel f <$> dupChan chan

-- | Read the next value from a 'Channel'
readChannel :: Channel s a -> IO a
readChannel c@(MkChannel chan f) = f <$> readChan chan >>= \case
                        Just v  -> return v
                        Nothing -> readChannel c

-- | Write a value to a 'Channel'
writeChannel :: Channel s a -> s -> IO ()
writeChannel (MkChannel chan _) = writeChan chan

-- | Transform a 'Channel' into one yielding another type. This function can
--   also be used to filter over 'Channel's.
transform :: (a -> Maybe b) -> Channel s a -> Channel s b
transform f (MkChannel chan g) = MkChannel chan (g >=> f)

instance Functor (Channel s) where
  fmap f = transform (Just . f)

-- | Merge two 'Channel's with the same origin into one.
--
--   ATTENTION: Don't merge channels with different sources.
mergeChannel :: Channel s a -> Channel s b -> Channel s (Either a b)
mergeChannel (MkChannel chan f) (MkChannel chan' g)
  | chan /= chan' = error "Trying to merge two Channels with different sources"
  | otherwise     = MkChannel chan h
  where h s = Left <$> f s <|> Right <$> g s
