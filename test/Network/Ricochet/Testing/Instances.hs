{-|
  Module      : Network.Ricochet.Testing.Instances
  Description : Instances needed for testing

"Network.Ricochet.Testing.Instances" contains all of the instances needed for
proper testing. This will be mainly QuickCheck's Arbirtrary.
-}

module Network.Ricochet.Testing.Instances where

import Test.QuickCheck
import Data.ByteString

instance Arbitrary ByteString where
  arbitrary = pack <$> vectorOf 30 arbitrary
