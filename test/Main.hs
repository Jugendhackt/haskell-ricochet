{-|
  Module      : Main
  Description : The executable to run all of the tests

"Main" is the top-level testing module describing the executable running all of
the tests for the project.
-}

module Main where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import Network.Ricochet.Testing.Instances
import Network.Ricochet.Testing.Crypto

main = defaultMain tests

tests = testGroup "Network.Ricochet.Testing" [ cryptoChecks ]

cryptoChecks = testGroup "Network.Ricochet.Testing.Crypto" [
    QC.testProperty "base64check: en- and decoding works" base64Check
  , QC.testProperty "signCheck: signing and verifying works" signCheck
  ]
