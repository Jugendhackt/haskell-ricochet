{-|
  Module      : Main
  Description : The executable to run all of the tests

"Main" is the top-level testing module describing the executable running all of
the tests for the project.
-}

module Main where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import Network.Ricochet.Testing.Instances
import Network.Ricochet.Testing.Crypto
import Network.Ricochet.Testing.General

main = defaultMain tests

tests = testGroup "Network.Ricochet.Testing" [ cryptoTests, generalTests ]

cryptoTests = testGroup "Network.Ricochet.Testing.Crypto"
  [ QC.testProperty "base64check: en- and decoding works" base64Check
  , rsaTests
  ]

generalTests = testGroup "Network.Ricochet.Testing.General"
  [ HU.testCase
      "connectionAssertion: connections can be established and version negotiation works"
      connectionAssertion
  ]
