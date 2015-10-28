{-|
  Module:      Network.Ricochet.Protocol.Protobuf.ContactRequest
  Description: Lenses for ContactRequestChannel.proto messages

"Network.Ricochet.Protocol.Protobuf.ContactRequest" re-exports lenses generated
by hprotoc.
-}

module Network.Ricochet.Protocol.Protobuf.ContactRequest
  ( contact_request
  , response
  , Limits (..)
  , ContactRequest (ContactRequest)
  , nickname
  , message_text
  , Response (Response)
  , status
  , Status (..)
  ) where

import           Network.Ricochet.Protocol.Data.ContactRequest
import           Network.Ricochet.Protocol.Data.ContactRequest.ContactRequest
import           Network.Ricochet.Protocol.Data.ContactRequest.Limits
import           Network.Ricochet.Protocol.Data.ContactRequest.Response
import           Network.Ricochet.Protocol.Data.ContactRequest.Response.Status

contact_request = _contact_request
response = _response
