{-# LANGUAGE ForeignFunctionInterface #-}
{-|
  Module: Network.Ricochet.Crypto.RSA
  Description: DER-encoding and decoding of RSA keys compatible with HsOpenSSL

This module binds to the parts of OpenSSL required for DER-encoding and decoding
RSA keys.  We have sent a PR to the HsOpenSSL project, but it has not been
accepted yet.
-}

module Network.Ricochet.Crypto.RSA
    ( fromDERPub
    , toDERPub
    , fromDERPriv
    , toDERPriv
    , rawRSASign
    , rawRSAVerify
    )
    where

#include <openssl/rsa.h>
#include <openssl/x509.h>

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B  (useAsCStringLen)
import qualified Data.ByteString.Internal   as BI (createAndTrim)
import           Foreign.ForeignPtr         (ForeignPtr, finalizeForeignPtr,
                                             newForeignPtr, withForeignPtr)
import           Foreign.Ptr                (FunPtr, Ptr, freeHaskellFunPtr,
                                             nullFunPtr, nullPtr)
import           Foreign.C.String           (CString)
import           Foreign.C.Types            (CLong(..), CInt(..), CUInt(..))
import           Foreign.Marshal.Alloc      (alloca)
import           Foreign.Storable           (peek, poke)
import           GHC.Word                   (Word8)
import           OpenSSL.EVP.Verify         (VerifyStatus(..))
import           OpenSSL.RSA                (RSA, RSAKey, RSAKeyPair, RSAPubKey,
                                             absorbRSAPtr, rsaSize, withRSAPtr)
import           System.IO.Unsafe           (unsafePerformIO)

type CDecodeFun = Ptr (Ptr RSA) -> Ptr CString -> CLong -> IO (Ptr RSA)
type CEncodeFun = Ptr RSA -> Ptr (Ptr Word8) -> IO CInt

foreign import ccall unsafe "d2i_RSAPublicKey"
  _fromDERPub :: CDecodeFun

foreign import ccall unsafe "i2d_RSAPublicKey"
  _toDERPub :: CEncodeFun

foreign import ccall unsafe "d2i_RSAPrivateKey"
  _fromDERPriv :: CDecodeFun

foreign import ccall unsafe "i2d_RSAPrivateKey"
  _toDERPriv :: CEncodeFun

-- | Generate a function that decodes a key from ASN.1 DER format
makeDecodeFun :: RSAKey k => CDecodeFun -> ByteString -> Maybe k
makeDecodeFun fun bs = unsafePerformIO . usingConvedBS $ \(csPtr, ci) -> do
  -- When you pass a null pointer to this function, it will allocate the memory
  -- space required for the RSA key all by itself.  It will be freed whenever
  -- the haskell object is garbage collected, as they are stored in ForeignPtrs
  -- internally.
  rsaPtr <- fun nullPtr csPtr ci
  if rsaPtr == nullPtr then return Nothing else absorbRSAPtr rsaPtr
  where usingConvedBS io = B.useAsCStringLen bs $ \(cs, len) ->
          alloca $ \csPtr -> poke csPtr cs >> io (csPtr, fromIntegral len)

-- | Generate a function that encodes a key in ASN.1 DER format
makeEncodeFun :: RSAKey k => CEncodeFun -> k -> ByteString
makeEncodeFun fun k = unsafePerformIO $ do
  -- When you pass a null pointer to this function, it will only compute the
  -- required buffer size.  See https://www.openssl.org/docs/faq.html#PROG3
  requiredSize <- withRSAPtr k $ flip fun nullPtr
  -- It’s too sad BI.createAndTrim is considered internal, as it does a great
  -- job here.  See https://hackage.haskell.org/package/bytestring-0.9.1.4/docs/Data-ByteString-Internal.html#v%3AcreateAndTrim
  BI.createAndTrim (fromIntegral requiredSize) $ \ptr ->
    alloca $ \pptr ->
      (fromIntegral <$>) . withRSAPtr k $ \key ->
        poke pptr ptr >> fun key pptr

-- | Dump a public key to ASN.1 DER format
toDERPub :: RSAKey k
         => k          -- ^ You can pass either 'RSAPubKey' or 'RSAKeyPair'
                       --   because both contain the necessary information.
         -> ByteString -- ^ The public key information encoded in ASN.1 DER
toDERPub = makeEncodeFun _toDERPub

-- | Parse a public key from ASN.1 DER format
fromDERPub :: ByteString -> Maybe RSAPubKey
fromDERPub = makeDecodeFun _fromDERPub

-- | Dump a private key to ASN.1 DER format
toDERPriv :: RSAKeyPair -> ByteString
toDERPriv = makeEncodeFun _toDERPriv

-- | Parse a private key from ASN.1 DER format
fromDERPriv :: RSAKey k
            => ByteString -- ^ The private key information encoded in ASN.1 DER
            -> Maybe k    -- ^ This can return either 'RSAPubKey' or
                          --   'RSAKeyPair' because there’s sufficient
                          --   information for both.
fromDERPriv = makeDecodeFun _fromDERPriv

foreign import ccall unsafe "RSA_sign"
  _rsa_sign :: CInt -> CString -> CUInt -> Ptr Word8 -> Ptr CUInt -> Ptr RSA -> IO CInt

foreign import ccall unsafe "RSA_verify"
  _rsa_verify :: CInt -> CString -> CUInt -> CString -> CUInt -> Ptr RSA -> IO CInt

_nid_sha256 :: CInt
_nid_sha256 = #const NID_sha256

-- | Sign a hash digest using the given RSA key, assuming the digest was
--   produced with sha256.
rawRSASign :: RSAKeyPair -> ByteString -> ByteString
rawRSASign k bs = unsafePerformIO $ do
  -- The signature is of the same length as the key modulus
  let requiredSize = rsaSize k
  BI.createAndTrim (fromIntegral requiredSize) $ \ptr ->
    (fromIntegral <$>) . alloca $ \iptr ->
      B.useAsCStringLen bs $ \(cs, len) ->
        withRSAPtr k $ \key ->
          poke iptr 0 >>
            _rsa_sign _nid_sha256 cs (fromIntegral len) ptr iptr key >>
              peek iptr

-- | Verify that a signature was created using the given hash digest and the
--   given RSA key, assuming the digest was produced with sha256.
rawRSAVerify :: RSAKey k => k -> ByteString -> ByteString -> VerifyStatus
rawRSAVerify k dig sig = toVerifyStatus . (== 1) . unsafePerformIO $ do
  withRSAPtr k $ \key ->
    B.useAsCStringLen dig $ \(cdig, dlen) ->
      B.useAsCStringLen sig $ \(csig, slen) ->
        _rsa_verify _nid_sha256 cdig (fromIntegral dlen) csig (fromIntegral slen) key
  where toVerifyStatus True = VerifySuccess
        toVerifyStatus False = VerifyFailure
