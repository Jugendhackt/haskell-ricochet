{-|
  Module:      Network.Ricochet.Protocol.Protobuf.ContactRequest
  Description: Lenses for ContactRequestChannel.proto messages

These types and lenses are useful to deal with protobuf messages sent in
relation with @im.ricochet.contact.request@ channels.  They are used te
introduce a new client and ask for user approval to send messages.
@im.ricochet.auth.hidden-service@ authentication should be established
beforehand.
-}

module Network.Ricochet.Protocol.Protobuf.ContactRequest
  ( contact_request
  , response
  , CR.ContactRequest (CR.ContactRequest)
  , nickname
  , message_text
  , R.Response (R.Response)
  , R.status
  , RS.Status (..)
  , nicknameMaxCharacters
  , messageMaxCharacters
  ) where

import qualified Network.Ricochet.Protocol.Data.ContactRequest                 as CRE
import qualified Network.Ricochet.Protocol.Data.ContactRequest.ContactRequest  as CR
import           Network.Ricochet.Protocol.Data.ContactRequest.Limits
import qualified Network.Ricochet.Protocol.Data.ContactRequest.Response        as R
import qualified Network.Ricochet.Protocol.Data.ContactRequest.Response.Status as RS
import           Network.Ricochet.Protocol.Data.Control.ChannelResult (ChannelResult)
import           Network.Ricochet.Protocol.Data.Control.OpenChannel (OpenChannel)

import           Network.Ricochet.Protocol.Protobuf (ext, utf8')

import           Control.Lens                     (Lens', Traversal', _Just)
import           Data.Text                        (Text)

-- | Request a hidden service @onion@ domain to be added to the recipient’s
--   contact list.  This will usually prompt the recipient user.
contact_request :: Traversal' OpenChannel (Maybe CR.ContactRequest)
contact_request = ext CRE._contact_request

-- | Respond to a contact request, informing the recipient in what status the
--   request is.
response :: Traversal' ChannelResult (Maybe R.Response)
response = ext CRE._response

-- | An optional nickname included in the contact request, that will be shown to
--   the recipient user.  It is limited to 'nicknameMaxCharacters' characters.
nickname :: Traversal' CR.ContactRequest Text
nickname = CR.nickname . _Just . utf8'

-- | An optional message text included in the contact request, that will be
--   shown to the recipient user.  It is limited to 'messageMaxCharacters'
--   characters.
message_text :: Traversal' CR.ContactRequest Text
message_text = CR.message_text . _Just . utf8'

-- | The maximum amount of characters that is allowed in a nickname.  This value
--   is specified in the protocol buffer specification files.
nicknameMaxCharacters :: Int
nicknameMaxCharacters = fromEnum NicknameMaxCharacters

-- | The maximum amount of characters that is allowed in a message.  This value
--   is specified in the protocol buffer specification files.
messageMaxCharacters :: Int
messageMaxCharacters = fromEnum MessageMaxCharacters
