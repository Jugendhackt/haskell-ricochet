{-|
  Module      : Network.Ricochet.Testing.Crypto
  Description : The tests for Network.Ricochet.Crypto

"Network.Ricochet.Testing.Crypto" contains all of the tests for
"Network.Ricochet.Crypto".
-}

module Network.Ricochet.Testing.Crypto where

import Network.Ricochet.Crypto
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Maybe
import Data.ByteString
import Control.Lens
import OpenSSL.EVP.Verify hiding (verify)

-- | Check the base64 Prism
base64Check :: ByteString -> Bool
base64Check bs = let tested = bs ^? re base64 . base64
                 in  fromJust tested == bs

-- | Check the sign and verify functions
signCheck :: ByteString -> ByteString -> Property
signCheck bs bs' = monadicIO $ do
  key <- run generate1024BitRSA
  let signature    = sign key bs
  assert . verify key bs $ signature
  assert . not . verify key bs' $ signature
