{-|
  Module      : Network.Ricochet.Testing.General
  Description : The tests for the entirety of Network.Ricochet

"Network.Ricochet.Testing.General" contains tests making sure the entirety of
"Network.Ricochet" works as expected.
-}

module Network.Ricochet.Testing.General where

import Control.Lens
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B8
import Network hiding (connectTo)
import Network.Ricochet
import Network.Ricochet.Connection
import Network.Ricochet.Monad
import Test.HUnit

-- | Make sure connections can be established and version negotiation works
connectionAssertion :: Assertion
connectionAssertion = do
  mVar <- newEmptyMVar

  -- Start the server thread
  forkIO . startRicochet (PortNumber 9880) Nothing (PortNumber 9051) []
                         (PortNumber 9050) [(1, closeConnection)] $ do
    use hiddenDomain >>= liftIO . putMVar mVar
    awaitConnection >>= closeConnection

  -- Try connecting to it
  startRicochet (PortNumber 9881) Nothing (PortNumber 9051) []
                (PortNumber 9050) [(1, closeConnection)] $ do
    domain <- B8.unpack <$> (liftIO . readMVar $ mVar)
    connectTo domain (PortNumber 9880) >>= closeConnection
