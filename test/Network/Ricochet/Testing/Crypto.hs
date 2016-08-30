{-|
  Module      : Network.Ricochet.Testing.Crypto
  Description : The tests for Network.Ricochet.Crypto

"Network.Ricochet.Testing.Crypto" contains all of the tests for
"Network.Ricochet.Crypto".
-}

module Network.Ricochet.Testing.Crypto where

import Network.Ricochet
import Network.Ricochet.Monad
import Network.Ricochet.Crypto
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.HUnit hiding (assert)
import Data.Maybe
import Data.ByteString
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Network
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

-- | Check the raw sign and verify functions
rawSignCheck :: ByteString -> ByteString -> Property
rawSignCheck bs bs' = monadicIO $ do
  key <- run generate1024BitRSA
  let signature    = rawRSASign key bs
  assert . rawRSAVerify key bs $ signature
  assert . not . rawRSAVerify key bs' $ signature

-- | Assert that torDomain computes the correct domain
torDomainAssertion :: Assertion
torDomainAssertion = do
  key <- generate1024BitRSA
  mVar <- newEmptyMVar
  let pubKey = base64 . privateDER # key
  startRicochet (PortNumber 9879) (Just pubKey) (PortNumber 9051) []
                (PortNumber 9050) []
                (use hiddenDomain >>= liftIO . putMVar mVar)
  domain <- readMVar mVar
  assertEqual "" domain $ torDomain key
