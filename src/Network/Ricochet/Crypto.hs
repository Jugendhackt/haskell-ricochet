{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
  Module:      Network.Ricochet.Crypto
  Description: Cryptographic primitives used internally by the library

Since the cryptography libraries for haskell aren’t that great, we didn’t find
everything we need.  This module is a wrapper around the libraries we use, and
adds our own implementations/bindings of primitives that aren’t defined
elsewhere.
-}

module Network.Ricochet.Crypto
  ( generate1024BitRSA
  , hmacSHA256
  , hashSHA1
  , sign
  , verify
  , publicDER
  , privateDER
  , base64
  , public
  , torDomain
  )
where

import           Network.Ricochet.Crypto.RSA

import           Control.Lens               (Prism', Review, (^?), (#), _Right,
                                             prism', to, unto)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B (take)
import qualified Data.ByteString.Base32     as B32 (encode)
import           Data.ByteString.Base64     as B64 (encode, decode)
import qualified Data.ByteString.Char8      as B8 (map)
import           Data.Char                  (toLower)
import           Data.Maybe                 (fromJust)
import           OpenSSL                    (withOpenSSL)
import           OpenSSL.EVP.Digest         (Digest, getDigestByName, hmacBS,
                                             digestBS)
import           OpenSSL.EVP.PKey           (KeyPair, PublicKey)
import           OpenSSL.EVP.Sign           (signBS)
import           OpenSSL.EVP.Verify         (VerifyStatus, verifyBS)
import           OpenSSL.RSA                (RSAKey, RSAKeyPair, RSAPubKey,
                                             generateRSAKey', rsaCopyPublic)
import           System.IO.Unsafe           (unsafePerformIO)

-- | Generate a 1024 bit private RSA key.  The public exponent is 3.
generate1024BitRSA :: IO RSAKeyPair
generate1024BitRSA = generateRSAKey' 1024 3

-- | The SHA256 Hash algorithm
sha256 :: Digest
sha256 = fromJust . unsafePerformIO . withOpenSSL $ getDigestByName "SHA256"

-- | The SHA1 Hash algorithm
sha1 :: Digest
sha1 = fromJust . unsafePerformIO . withOpenSSL $ getDigestByName "SHA1"

-- | Perform a secret key signing using HMAC-SHA256
hmacSHA256 :: ByteString -- ^ The (shared) secret key
           -> ByteString -- ^ The data to be signed
           -> ByteString -- ^ The resulting HMAC signature
hmacSHA256 key bs = hmacBS sha256 key bs

-- | Hashes a 'ByteString' using 'sha1'.
hashSHA1 :: ByteString -> ByteString
hashSHA1 = digestBS sha1

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

-- | A Prism that allows ASN.1 DER encoding and decoding of RSA public keys
publicDER :: Prism' ByteString RSAPubKey
publicDER = prism' toDERPub fromDERPub

-- | A Prism that allows ASN.1 DER encoding and decoding of RSA private keys
privateDER :: Prism' ByteString RSAKeyPair
privateDER = prism' toDERPriv fromDERPriv

-- | A Prism that allows Base64 encoding and decoding of ByteStrings
base64 :: Prism' ByteString ByteString
base64 = prism' B64.encode (^? to B64.decode . _Right)

-- | This allows you to use an RSAKey where a RSAPubKey is required.
public :: RSAKey k => Review RSAPubKey k
public = unto $ unsafePerformIO . rsaCopyPublic

-- | Compute the Tor domain name of some RSA key
torDomain :: RSAKey k => k -> ByteString
torDomain k = B8.map toLower . B32.encode . B.take 10 . hashSHA1 $ publicDER . public # k
