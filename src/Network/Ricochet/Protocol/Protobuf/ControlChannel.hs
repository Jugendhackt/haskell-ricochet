module Network.Ricochet.Protocol.Protobuf.ControlChannel
  ( channel_identifier
  , feature
  , CP.Packet
  , open_channel
  , channel_result
  , keep_alive
  , enable_features
  , features_enabled
  , O.OpenChannel
  , O.channel_type
  , client_cookie
  , server_cookie
  , R.ChannelResult
  , R.opened
  , KeepAlive
  , response_requested
  ) where

import qualified Network.Ricochet.Protocol.Data.Control.Packet as CP
import qualified Network.Ricochet.Protocol.Data.Control.OpenChannel as O
import qualified Network.Ricochet.Protocol.Data.Control.ChannelResult as R
import           Network.Ricochet.Protocol.Data.Control.KeepAlive
import qualified Network.Ricochet.Protocol.Data.Control.EnableFeatures as E
import qualified Network.Ricochet.Protocol.Data.Control.FeaturesEnabled as F
import           Network.Ricochet.Protocol.Data.AuthHiddenService

import           Control.Lens
import           Control.Monad
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Lens
import           GHC.Int                          (Int32)
import           Text.ProtocolBuffers

open_channel :: Traversal' CP.Packet O.OpenChannel
open_channel = CP.open_channel . _Just

channel_result :: Traversal' CP.Packet R.ChannelResult
channel_result = CP.channel_result . _Just

keep_alive :: Traversal' CP.Packet KeepAlive
keep_alive = CP.keep_alive . _Just

enable_features :: Traversal' CP.Packet E.EnableFeatures
enable_features = CP.enable_features . _Just

features_enabled :: Traversal' CP.Packet F.FeaturesEnabled
features_enabled = CP.features_enabled . _Just

class HasChannelIdentifier m where
  channel_identifier :: Lens' m Int32

instance HasChannelIdentifier O.OpenChannel where
  channel_identifier = O.channel_identifier

instance HasChannelIdentifier R.ChannelResult where
  channel_identifier = R.channel_identifier

class HasFeature m where
  feature :: Lens' m (Seq Utf8)

instance HasFeature E.EnableFeatures where
  feature = E.feature

instance HasFeature F.FeaturesEnabled where
  feature = F.feature

client_cookie = _client_cookie
server_cookie = _server_cookie
