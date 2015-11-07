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
  )
where

import           Network.Ricochet.Crypto.RSA
