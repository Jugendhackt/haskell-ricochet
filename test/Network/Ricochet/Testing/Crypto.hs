{-|
  Module      : Network.Ricochet.Testing.Crypto
  Description : The tests for Network.Ricochet.Crypto

"Network.Ricochet.Testing.Crypto" contains all of the tests for
"Network.Ricochet.Crypto".
-}

{-# LANGUAGE TupleSections #-}
module Network.Ricochet.Testing.Crypto where

import Network.Ricochet
import Network.Ricochet.Crypto
import Network.Ricochet.Monad
import Network.Ricochet.Testing.Instances

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.HUnit hiding (assert)
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Network
import OpenSSL.EVP.Verify hiding (verify)
import OpenSSL.RSA (RSAKeyPair)

-- TODO: Extract this into some general utility
type RSAPool = M.Map RSATestIdent (Chan RSAKeyPair)

data RSATestIdent = RandomSigs | CorrectSigs | RawRandomSigs | RawCorrectSigs | TorDomainAssertion
  deriving (Bounded, Enum, Eq, Ord)

newRSAPool :: IO RSAPool
newRSAPool = do
  chanMap <- fmap M.fromList . forM ([minBound..maxBound] :: [RSATestIdent]) $
    \k -> (k,) <$> newChan
  -- FIXME: Keys should be generated on demand:  We shouldn’t specify the total
  -- number of generated keys (100) here.
  forkIO . void . forM [1..100] . const $ do
    key <- generate1024BitRSA
    forM chanMap (flip writeChan key)
  return chanMap

getRSAKeyPair :: IO RSAPool -> RSATestIdent -> IO RSAKeyPair
getRSAKeyPair poolAction ident = do
  pool <- poolAction
  readChan $ pool M.! ident

-- | Check the base64 Prism
base64Check :: ByteString -> Bool
base64Check bs = let tested = bs ^? re base64 . base64
                 in  fromJust tested == bs

-- | All the tests that need 1024 bit RSA keys
rsaTests :: TestTree
rsaTests = withResource newRSAPool (const $ return ()) rsaTests'

rsaTests' :: IO RSAPool -> TestTree
rsaTests' pool = testGroup "Network.Ricochet.Testing.Crypto.RSA"
  [ QC.testProperty "randomSigs: verifying random signatures fails" (randomSigs pool)
  , QC.testProperty "correctSigs: verifying correct signatures succeeds" (correctSigs pool)
  , QC.testProperty "rawRandomSigs: verifying random raw signatures fails" (rawRandomSigs pool)
  , QC.testProperty "rawCorrectSigs: verifying correct raw signatures succeeds" (rawCorrectSigs pool)
  , HU.testCase "torDomainAssertion: hidden service domains are computed correctly" (torDomainAssertion pool)
  ]

-- | Verifying random signatures fails
randomSigs :: IO RSAPool -> ByteString -> ByteString -> Property
randomSigs pool bs bs' = monadicIO $ do
  key <- run $ getRSAKeyPair pool RandomSigs
  let signature    = sign key bs
  assert . not . verify key bs' $ signature

-- | Verifying correct signatures succeeds
correctSigs :: IO RSAPool -> ByteString -> Property
correctSigs pool bs = monadicIO $ do
  key <- run $ getRSAKeyPair pool CorrectSigs
  let signature    = sign key bs
  assert . verify key bs $ signature

-- | Verifying random raw signatures fails
rawRandomSigs :: IO RSAPool -> ByteString -> ByteString -> Property
rawRandomSigs pool bs bs' = monadicIO $ do
  key <- run $ getRSAKeyPair pool RawRandomSigs
  let signature    = rawRSASign key bs
  assert . not . rawRSAVerify key bs' $ signature

-- | Verifying correct raw signatures succeeds
rawCorrectSigs :: IO RSAPool -> ByteString -> Property
rawCorrectSigs pool bs = monadicIO $ do
  key <- run $ getRSAKeyPair pool RawCorrectSigs
  let signature    = rawRSASign key bs
  assert . rawRSAVerify key bs $ signature

-- | Assert that torDomain computes the correct domain
torDomainAssertion :: IO RSAPool -> Assertion
torDomainAssertion pool = do
  key <- getRSAKeyPair pool TorDomainAssertion
  mVar <- newEmptyMVar
  let pubKey = base64 . privateDER # key
  startRicochet (PortNumber 9879) (Just pubKey) (PortNumber 9051) []
                (PortNumber 9050) []
                (use hiddenDomain >>= liftIO . putMVar mVar)
  domain <- readMVar mVar
  assertEqual "" domain $ torDomain key
