{-|
  Module      : Main
  Description : The executable to run all of the tests

"Main" is the top-level testing module describing the executable running all of
the tests for the project.
-}

module Main where

import Test.Tasty

main = defaultMain tests

tests = testGroup "Network.Ricochet.Testing" []
