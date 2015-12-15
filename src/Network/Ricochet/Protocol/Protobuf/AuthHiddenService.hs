{-|
  Module:      Network.Ricochet.Protocol.Protobuf.AuthHiddenService
  Description: AuthHiddenService.proto messages and their lenses

These types and lenses are useful for dealing with protobuf messages sent in
relation with @im.ricochet.auth.hidden-service@ channels.  They are used to
prove ownership of a hidden service name by demonstrating ownership of a
matching private key.  This is used to authenticate as a known contact, or to
prove ownership of a service name before sending a contact request.

See <https://github.com/ricochet-im/ricochet/blob/master/doc/protocol.md#authhiddenservice ricochet’s protocol specification>.
-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.Ricochet.Protocol.Protobuf.AuthHiddenService
  ( client_cookie
  , server_cookie
  , AP.Packet (AP.Packet)
  , proof
  , result
  , AO.Proof (AO.Proof)
  , public_key
  , signature
  , AR.Result (AR.Result)
  , accepted
  , is_known_contact
  ) where

import           Network.Ricochet.Protocol.Data.AuthHiddenService
import qualified Network.Ricochet.Protocol.Data.AuthHiddenService.Packet as AP
import qualified Network.Ricochet.Protocol.Data.AuthHiddenService.Proof  as AO
import qualified Network.Ricochet.Protocol.Data.AuthHiddenService.Result as AR
import           Network.Ricochet.Protocol.Data.Control.ChannelResult (ChannelResult)
import           Network.Ricochet.Protocol.Data.Control.OpenChannel (OpenChannel)

import           Network.Ricochet.Protocol.Protobuf (ext)

import           Control.Lens                     (Lens', Traversal', _Just,
                                                   strict)
import           Data.ByteString                  (ByteString)

-- | The client’s part of the random string that will be used as an input to
--   calculate the proof.
client_cookie = ext _client_cookie

-- | The server’s part of the random string that will be used as an input to
--   calculate the proof.
server_cookie = ext _server_cookie

-- | If the 'AP.Packet' came from the client, it /must/ contain a 'Proof'.  It
--   is used to prove the ownership of a hidden service to the server.
proof :: Traversal' AP.Packet AO.Proof
proof = AP.proof . _Just

-- | If the 'AP.Packet' came from the server, it /must/ contain a 'Result'.  It
--   is used to tell the client whether it’s proof has been accepted, and
--   whether the hidden service is a known contact.
result :: Traversal' AP.Packet AR.Result
result = AP.result . _Just

-- | The public key corresponding to the hidden service the client wants to
--   prove ownership of.  It has to be a DER-encoded 1024-bit RSA public key.
--   It will be used to calculate the @onion@ address and verify the
--   'signature'.
public_key :: Traversal' AO.Proof ByteString
public_key = AO.public_key . _Just . strict

-- | The signature of the HMAC-SHA256 proof, signed by the client’s hidden
--   service’s private key.
signature :: Traversal' AO.Proof ByteString
signature = AO.signature . _Just . strict

-- | Whether the public key was decoded and the signature verified successfully.
--   If this is true, the client has @im.ricochet.auth.hidden-service@
--   authentication.
accepted :: Lens' AR.Result Bool
accepted = AR.accepted

-- | Whether the client is a known contact already.  This will only be true if
--   'accepted' is already true.  In that case, the client has
--   @im.ricochet.auth.hidden-service@ authentication as a known contact.
is_known_contact :: Lens' AR.Result (Maybe Bool)
is_known_contact = AR.is_known_contact
