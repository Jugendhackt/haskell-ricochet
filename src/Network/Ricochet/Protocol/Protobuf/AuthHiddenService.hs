module Network.Ricochet.Protocol.Protobuf.AuthHiddenService
  ( client_cookie
  , server_cookie
  , AP.Packet
  , proof
  , result
  , Proof
  , public_key
  , signature
  , Result
  , accepted
  , is_known_contact
  ) where

import           Network.Ricochet.Protocol.Data.AuthHiddenService
import qualified Network.Ricochet.Protocol.Data.AuthHiddenService.Packet as AP
import           Network.Ricochet.Protocol.Data.AuthHiddenService.Proof
import           Network.Ricochet.Protocol.Data.AuthHiddenService.Result

import           Control.Lens
import           Control.Monad
import           Data.ByteString                  (ByteString)
import           Text.ProtocolBuffers

client_cookie = _client_cookie
server_cookie = _server_cookie

proof :: Traversal' AP.Packet Proof
proof = AP.proof . _Just

result :: Traversal' AP.Packet Result
result = AP.result . _Just
