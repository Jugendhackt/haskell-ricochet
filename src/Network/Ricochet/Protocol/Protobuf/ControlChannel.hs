{-|
  Module:      Network.Ricochet.Protocol.Protobuf.ControlChannel
  Description: Lenses for ControlChannel.proto messages

These types and lenses are useful for dealing with packets sent in control
channels.  It’s the only channel open from the beginning of a connection, and
can thus be used to open the other channels.  Additionally, you can send
'K.KeepAlive' packets and enable protocol extensions using 'E.EnableFeatures'.
-}

module Network.Ricochet.Protocol.Protobuf.ControlChannel
  ( CP.Packet (CP.Packet)
  -- | Every 'CP.Packet' should contain such data that only one of the following
  --   traversals will yield a result:
  , open_channel
  , channel_result
  , keep_alive
  , enable_features
  , features_enabled
  , channel_identifier
  , O.OpenChannel
  , channel_type
  , R.ChannelResult
  , opened
  , common_error
  , CE.CommonError (..)
  , K.KeepAlive
  , response_requested
  , E.EnableFeatures (E.EnableFeatures)
  , F.FeaturesEnabled (F.FeaturesEnabled)
  , feature
  ) where

import qualified Network.Ricochet.Protocol.Data.Control.Packet          as CP
import qualified Network.Ricochet.Protocol.Data.Control.OpenChannel     as O
import qualified Network.Ricochet.Protocol.Data.Control.ChannelResult   as R
import qualified Network.Ricochet.Protocol.Data.Control.ChannelResult.CommonError as CE
import qualified Network.Ricochet.Protocol.Data.Control.KeepAlive       as K
import qualified Network.Ricochet.Protocol.Data.Control.EnableFeatures  as E
import qualified Network.Ricochet.Protocol.Data.Control.FeaturesEnabled as F

import           Network.Ricochet.Protocol.Protobuf (utf8', i32)
import           Network.Ricochet.Types           (ChannelType(..))

import           Control.Lens                     (Lens', Traversal', _Just,
                                                   below, iso)
import           Data.ByteString                  (ByteString)
import           Data.Text                        (Text)
import           GHC.Word                         (Word16)
import           Text.ProtocolBuffers             (Seq)

-- | A request to to open an additional channel.  The receiver should check its
--   validity and reply with a 'R.ChannelResult' message.
open_channel :: Lens' CP.Packet (Maybe O.OpenChannel)
open_channel = CP.open_channel

-- | Response to an 'O.OpenChannel' message, telling the receiver whether the
--   channel is ready for use, or what has gone wrong.
channel_result :: Lens' CP.Packet (Maybe R.ChannelResult)
channel_result = CP.channel_result

-- | A ping/pong message. This can be used to ping the remote side, ie. to find
--   out how much latency the connection has.
keep_alive :: Lens' CP.Packet (Maybe K.KeepAlive)
keep_alive = CP.keep_alive

-- | Request to activate protocol extension features.  The remote side has to
--   respond with a 'F.FeaturesEnabled' message.
enable_features :: Lens' CP.Packet (Maybe E.EnableFeatures)
enable_features = CP.enable_features

-- | Response to an 'E.EnableFeatures' message, telling the receiver which of
--   the requested features have been enabled.
features_enabled :: Lens' CP.Packet (Maybe F.FeaturesEnabled)
features_enabled = CP.features_enabled

class HasChannelIdentifier m where
  -- | We use the typeclass 'HasChannelIdentifier' because both 'O.OpenChannel'
  --   and 'R.ChannelResult'  have a @channel_identifier@.
  --
  --   The channel identifier of either a 'O.OpenChannel' or a 'R.ChannelResult'
  --   message.  It is used to correlate both packets with a channel and a
  --   ChannelResult message with the OpenChannel one.
  --
  --   The standard specifies several rules to follow when choosing or accepting
  --   a channel identifier:
  --
  --   * The client side of a connection may only open odd-numbered channels
  --   * The server side may only open even-numbered channels
  --   * The identifier must fit within the range of uint16
  --   * The identifier must not be used by an open channel
  --   * The identifier should increase for every OpenChannel message, wrapping
  --     if necessary.  Identifiers should not be re-used except after wrapping.
  channel_identifier :: Lens' m Word16

instance HasChannelIdentifier O.OpenChannel where
  channel_identifier = O.channel_identifier . iso fromIntegral fromIntegral

instance HasChannelIdentifier R.ChannelResult where
  channel_identifier = R.channel_identifier . iso fromIntegral fromIntegral

-- | The type of the requested channel.  By convention, it is in reverse URI
--   format, e.g. @im.ricochet.chat@.  It specifies what kind of extensions to
--   the 'O.OpenChannel' and 'R.ChannelResult' messages are allowed, and what
--   kind of packets will be sent in the channel.
channel_type :: Traversal' O.OpenChannel ChannelType
channel_type = O.channel_type . utf8' . iso MkChannelType fromChannelType
  where fromChannelType (MkChannelType t) = t

-- | Whether the requested channel is now open and ready to receive packets.
opened :: Lens' R.ChannelResult Bool
opened = R.opened

-- | The error code that describes why the channel cannot be opened.
common_error :: Lens' R.ChannelResult (Maybe CE.CommonError)
common_error = R.common_error

-- | Whether this ping should be answered with a pong.  In other words, the
--   remote side will reply to a 'K.KeepAlive' message with 'response_requested'
--   set to True (=ping) with a 'K.KeepAlive' message with 'response_requested'
--   set to False (=pong).
response_requested :: Lens' K.KeepAlive Bool
response_requested = K.response_requested

class HasFeature m where
  -- | We use the typeclass 'HasFeature' because both 'E.EnableFeatures' and
  --   'F.FeaturesEnabled' have @feature@s.
  --
  --   In the request, this is the list of features that one side of the
  --   connection wishes to enable.  In the response, this will be all of the
  --   features supported by the recipient of the request, that are now enabled.
  feature :: Traversal' m (Seq Text)

instance HasFeature E.EnableFeatures where
  feature = E.feature . below utf8'

instance HasFeature F.FeaturesEnabled where
  feature = F.feature . below utf8'
