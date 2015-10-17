{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.Control.ChannelResult (ChannelResult(..)) where
import Prelude ((+), (/), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Protocol.Data.Control.ChannelResult.CommonError as Protocol.Data.Control.ChannelResult (CommonError)
 
data ChannelResult = ChannelResult{channel_identifier :: !(P'.Int32), opened :: !(P'.Bool),
                                   common_error :: !(P'.Maybe Protocol.Data.Control.ChannelResult.CommonError),
                                   ext'field :: !(P'.ExtField)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.ExtendMessage ChannelResult where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.Mergeable ChannelResult where
  mergeAppend (ChannelResult x'1 x'2 x'3 x'4) (ChannelResult y'1 y'2 y'3 y'4)
   = ChannelResult (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default ChannelResult where
  defaultValue = ChannelResult P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire ChannelResult where
  wireSize ft' self'@(ChannelResult x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeReq 1 8 x'2 + P'.wireSizeOpt 1 14 x'3 + P'.wireSizeExtField x'4)
  wirePut ft' self'@(ChannelResult x'1 x'2 x'3 x'4)
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
             P'.wirePutReq 16 8 x'2
             P'.wirePutOpt 24 14 x'3
             P'.wirePutExtField x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{channel_identifier = new'Field}) (P'.wireGet 5)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{opened = new'Field}) (P'.wireGet 8)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{common_error = Prelude'.Just new'Field}) (P'.wireGet 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [100 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ChannelResult) ChannelResult where
  getVal m' f' = f' m'
 
instance P'.GPB ChannelResult
 
instance P'.ReflectDescriptor ChannelResult where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16]) (P'.fromDistinctAscList [8, 16, 24])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.Control.ChannelResult\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\"], baseName = MName \"ChannelResult\"}, descFilePath = [\"Protocol\",\"Data\",\"Control\",\"ChannelResult.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.ChannelResult.channel_identifier\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"ChannelResult\"], baseName' = FName \"channel_identifier\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.ChannelResult.opened\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"ChannelResult\"], baseName' = FName \"opened\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.ChannelResult.common_error\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"ChannelResult\"], baseName' = FName \"common_error\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.Control.ChannelResult.CommonError\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"ChannelResult\"], baseName = MName \"CommonError\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 100},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ChannelResult where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ChannelResult where
  textPut msg
   = do
       P'.tellT "channel_identifier" (channel_identifier msg)
       P'.tellT "opened" (opened msg)
       P'.tellT "common_error" (common_error msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'channel_identifier, parse'opened, parse'common_error]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'channel_identifier
         = P'.try
            (do
               v <- P'.getT "channel_identifier"
               Prelude'.return (\ o -> o{channel_identifier = v}))
        parse'opened
         = P'.try
            (do
               v <- P'.getT "opened"
               Prelude'.return (\ o -> o{opened = v}))
        parse'common_error
         = P'.try
            (do
               v <- P'.getT "common_error"
               Prelude'.return (\ o -> o{common_error = v}))