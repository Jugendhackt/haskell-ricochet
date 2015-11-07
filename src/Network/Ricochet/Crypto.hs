{-|
  Module:      Network.Ricochet.Crypto
  Description: Cryptographic primitives used internally by the library

Since the cryptography libraries for haskell aren’t that great, we didn’t find
everything we need.  This module is a wrapper around the libraries we use, and
adds our own implementations/bindings of primitives that aren’t defined
elsewhere.
-}

module Network.Ricochet.Crypto
  ( module Network.Ricochet.Crypto.RSA
  , generate1024BitRSA
  , base64EncodePrivRSA
  , hmacSHA256
  , rsaSign
  )
where

import           Network.Ricochet.Crypto.RSA

import           Data.ByteString            (ByteString)
import           Data.Maybe                 (fromJust)
import           OpenSSL                    (withOpenSSL)
import           OpenSSL.EVP.Base64         (encodeBase64BS)
import           OpenSSL.EVP.Digest         (Digest, getDigestByName, hmacBS)
import           OpenSSL.EVP.Sign           (signBS)
import           OpenSSL.RSA                (RSAKeyPair, generateRSAKey')
import           System.IO.Unsafe           (unsafePerformIO)

-- | Generate a 1024 bit private RSA key.  The public exponent is 3.
generate1024BitRSA :: IO RSAKeyPair
generate1024BitRSA = generateRSAKey' 1024 3

-- | Encode a private RSA key in DER and then Base64
base64EncodePrivRSA :: RSAKeyPair -> ByteString
base64EncodePrivRSA = encodeBase64BS . toDERPriv

-- | The SHA256 Hash algorithm
sha256 :: Digest
sha256 = fromJust . unsafePerformIO . withOpenSSL $ getDigestByName "SHA256"

-- | Perform a secret key signing using HMAC-SHA256
hmacSHA256 :: ByteString -- ^ The (shared) secret key
           -> ByteString -- ^ The data to be signed
           -> ByteString -- ^ The resulting HMAC signature
hmacSHA256 key bs = hmacBS sha256 key bs

-- | Sign a ByteString using PKCS # 1 v2.0, using SHA256
rsaSign :: RSAKeyPair -> ByteString -> ByteString
rsaSign key bs = unsafePerformIO $ signBS sha256 key bs
