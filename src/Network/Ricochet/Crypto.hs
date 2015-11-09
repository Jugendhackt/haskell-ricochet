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
  , sign
  , verify
  )
where

import           Network.Ricochet.Crypto.RSA

import           Data.ByteString            (ByteString)
import           Data.Maybe                 (fromJust)
import           OpenSSL                    (withOpenSSL)
import           OpenSSL.EVP.Base64         (encodeBase64BS)
import           OpenSSL.EVP.Digest         (Digest, getDigestByName, hmacBS)
import           OpenSSL.EVP.PKey           (KeyPair, PublicKey)
import           OpenSSL.EVP.Sign           (signBS)
import           OpenSSL.EVP.Verify         (VerifyStatus, verifyBS)
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

-- | Sign a ByteString using the algorithm corresponding to the passed KeyPair
--   and SHA256.
sign :: KeyPair k => k -> ByteString -> ByteString
sign key bs = unsafePerformIO $ signBS sha256 key bs

-- | Verify a signature using the algorithm corresponding to the passed KeyPair
--   and SHA256.
verify :: PublicKey k
          => k            -- ^ The public key (or keypair) that belongs to the
                          --   private key used to sign the message
          -> ByteString   -- ^ The message that was signed
          -> ByteString   -- ^ The signature that is to be verified
          -> VerifyStatus -- ^ Whether the signature is valid or not
verify key msg sig = unsafePerformIO $ verifyBS sha256 sig key msg
