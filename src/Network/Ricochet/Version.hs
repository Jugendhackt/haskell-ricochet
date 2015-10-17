module Network.Ricochet.Version
  (Versions()) where

import           Network.Ricochet.Monad
import           Network.Ricochet.Types

import           Data.Map               (Map ())
import           GHC.Word               (Word8 ())

-- | Each version (Word8) has its own Handler that takes a Connection which has
-- completed version negotiation and does Ricochet actions
type Versions = Map Word8 (Connection -> Ricochet ())
