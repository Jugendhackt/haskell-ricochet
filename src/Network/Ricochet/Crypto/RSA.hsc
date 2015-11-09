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
import           Foreign.C.Types            (CLong(..), CInt(..))
import           Foreign.Marshal.Alloc      (alloca)
import           Foreign.Storable           (poke)
import           GHC.Word                   (Word8)
import           OpenSSL.RSA
import           System.IO.Unsafe           (unsafePerformIO)

type CDecodeFun = Ptr (Ptr RSA) -> Ptr CString -> CLong -> IO (Ptr RSA)
type CEncodeFun = Ptr RSA -> Ptr (Ptr Word8) -> IO CInt

foreign import ccall unsafe "d2i_RSAPublicKey"
  _fromDERPub :: CDecodeFun

foreign import ccall unsafe "i2d_RSAPublicKey"
  _toDERPub :: CEncodeFun

makeDecodeFun :: RSAKey k => CDecodeFun -> ByteString -> Maybe k
makeDecodeFun fun bs = unsafePerformIO . usingConvedBS $ \(csPtr, ci) -> do
  rsaPtr <- fun nullPtr csPtr ci
  if rsaPtr == nullPtr then return Nothing else absorbRSAPtr rsaPtr
  where usingConvedBS io = B.useAsCStringLen bs $ \(cs, len) ->
          alloca $ \csPtr -> poke csPtr cs >> io (csPtr, fromIntegral len)

makeEncodeFun :: RSAKey k => CEncodeFun -> k -> ByteString
makeEncodeFun fun k = unsafePerformIO $ do
  requiredSize <- withRSAPtr k $ flip _toDERPub nullPtr
  BI.createAndTrim (fromIntegral requiredSize) $ \ptr ->
    alloca $ \pptr ->
      (fromIntegral <$>) $ withRSAPtr k $ \key ->
        poke pptr ptr >> fun key pptr

-- | Dump a public key to ASN.1 DER format
toDERPub :: RSAKey k => k -> ByteString
toDERPub = makeEncodeFun _toDERPub

-- | Parse a public key from ASN.1 DER format
fromDERPub :: ByteString -> Maybe RSAPubKey
fromDERPub = makeDecodeFun _fromDERPub

-- vim: ft=haskell
