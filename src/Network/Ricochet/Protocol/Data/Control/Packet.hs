{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.Control.Packet (Packet(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Protocol.Data.Control.ChannelResult as Protocol.Data.Control (ChannelResult)
import qualified Protocol.Data.Control.EnableFeatures as Protocol.Data.Control (EnableFeatures)
import qualified Protocol.Data.Control.FeaturesEnabled as Protocol.Data.Control (FeaturesEnabled)
import qualified Protocol.Data.Control.KeepAlive as Protocol.Data.Control (KeepAlive)
import qualified Protocol.Data.Control.OpenChannel as Protocol.Data.Control (OpenChannel)
 
data Packet = Packet{open_channel :: !(P'.Maybe Protocol.Data.Control.OpenChannel),
                     channel_result :: !(P'.Maybe Protocol.Data.Control.ChannelResult),
                     keep_alive :: !(P'.Maybe Protocol.Data.Control.KeepAlive),
                     enable_features :: !(P'.Maybe Protocol.Data.Control.EnableFeatures),
                     features_enabled :: !(P'.Maybe Protocol.Data.Control.FeaturesEnabled)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Packet where
  mergeAppend (Packet x'1 x'2 x'3 x'4 x'5) (Packet y'1 y'2 y'3 y'4 y'5)
   = Packet (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
 
instance P'.Default Packet where
  defaultValue = Packet P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Packet where
  wireSize ft' self'@(Packet x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeOpt 1 11 x'4 +
             P'.wireSizeOpt 1 11 x'5)
  wirePut ft' self'@(Packet x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 11 x'1
             P'.wirePutOpt 18 11 x'2
             P'.wirePutOpt 26 11 x'3
             P'.wirePutOpt 34 11 x'4
             P'.wirePutOpt 42 11 x'5
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{open_channel = P'.mergeAppend (open_channel old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{channel_result = P'.mergeAppend (channel_result old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{keep_alive = P'.mergeAppend (keep_alive old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             34 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{enable_features = P'.mergeAppend (enable_features old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             42 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{features_enabled = P'.mergeAppend (features_enabled old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Packet) Packet where
  getVal m' f' = f' m'
 
instance P'.GPB Packet
 
instance P'.ReflectDescriptor Packet where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26, 34, 42])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.Control.Packet\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\"], baseName = MName \"Packet\"}, descFilePath = [\"Protocol\",\"Data\",\"Control\",\"Packet.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.Packet.open_channel\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"Packet\"], baseName' = FName \"open_channel\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.Control.OpenChannel\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\"], baseName = MName \"OpenChannel\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.Packet.channel_result\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"Packet\"], baseName' = FName \"channel_result\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.Control.ChannelResult\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\"], baseName = MName \"ChannelResult\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.Packet.keep_alive\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"Packet\"], baseName' = FName \"keep_alive\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.Control.KeepAlive\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\"], baseName = MName \"KeepAlive\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.Packet.enable_features\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"Packet\"], baseName' = FName \"enable_features\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.Control.EnableFeatures\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\"], baseName = MName \"EnableFeatures\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.Packet.features_enabled\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"Packet\"], baseName' = FName \"features_enabled\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.Control.FeaturesEnabled\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\"], baseName = MName \"FeaturesEnabled\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType Packet where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg Packet where
  textPut msg
   = do
       P'.tellT "open_channel" (open_channel msg)
       P'.tellT "channel_result" (channel_result msg)
       P'.tellT "keep_alive" (keep_alive msg)
       P'.tellT "enable_features" (enable_features msg)
       P'.tellT "features_enabled" (features_enabled msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'open_channel, parse'channel_result, parse'keep_alive, parse'enable_features, parse'features_enabled])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'open_channel
         = P'.try
            (do
               v <- P'.getT "open_channel"
               Prelude'.return (\ o -> o{open_channel = v}))
        parse'channel_result
         = P'.try
            (do
               v <- P'.getT "channel_result"
               Prelude'.return (\ o -> o{channel_result = v}))
        parse'keep_alive
         = P'.try
            (do
               v <- P'.getT "keep_alive"
               Prelude'.return (\ o -> o{keep_alive = v}))
        parse'enable_features
         = P'.try
            (do
               v <- P'.getT "enable_features"
               Prelude'.return (\ o -> o{enable_features = v}))
        parse'features_enabled
         = P'.try
            (do
               v <- P'.getT "features_enabled"
               Prelude'.return (\ o -> o{features_enabled = v}))