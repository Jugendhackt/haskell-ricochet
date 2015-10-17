{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.Control.OpenChannel (OpenChannel(..)) where
import Prelude ((+), (/), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data OpenChannel = OpenChannel{channel_identifier :: !(P'.Int32), channel_type :: !(P'.Utf8), ext'field :: !(P'.ExtField)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.ExtendMessage OpenChannel where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.Mergeable OpenChannel where
  mergeAppend (OpenChannel x'1 x'2 x'3) (OpenChannel y'1 y'2 y'3)
   = OpenChannel (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default OpenChannel where
  defaultValue = OpenChannel P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire OpenChannel where
  wireSize ft' self'@(OpenChannel x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeReq 1 9 x'2 + P'.wireSizeExtField x'3)
  wirePut ft' self'@(OpenChannel x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 5 x'1
             P'.wirePutReq 18 9 x'2
             P'.wirePutExtField x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{channel_identifier = new'Field}) (P'.wireGet 5)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{channel_type = new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [100 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> OpenChannel) OpenChannel where
  getVal m' f' = f' m'
 
instance P'.GPB OpenChannel
 
instance P'.ReflectDescriptor OpenChannel where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 18]) (P'.fromDistinctAscList [8, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.Control.OpenChannel\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\"], baseName = MName \"OpenChannel\"}, descFilePath = [\"Protocol\",\"Data\",\"Control\",\"OpenChannel.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.OpenChannel.channel_identifier\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"OpenChannel\"], baseName' = FName \"channel_identifier\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.OpenChannel.channel_type\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"OpenChannel\"], baseName' = FName \"channel_type\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 100},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType OpenChannel where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg OpenChannel where
  textPut msg
   = do
       P'.tellT "channel_identifier" (channel_identifier msg)
       P'.tellT "channel_type" (channel_type msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'channel_identifier, parse'channel_type]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'channel_identifier
         = P'.try
            (do
               v <- P'.getT "channel_identifier"
               Prelude'.return (\ o -> o{channel_identifier = v}))
        parse'channel_type
         = P'.try
            (do
               v <- P'.getT "channel_type"
               Prelude'.return (\ o -> o{channel_type = v}))