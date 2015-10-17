{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.Chat.ChatMessage (ChatMessage(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ChatMessage = ChatMessage{message_text :: !(P'.Utf8), message_id :: !(P'.Maybe P'.Word32), time_delta :: !(P'.Maybe P'.Int64)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ChatMessage where
  mergeAppend (ChatMessage x'1 x'2 x'3) (ChatMessage y'1 y'2 y'3)
   = ChatMessage (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default ChatMessage where
  defaultValue = ChatMessage P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire ChatMessage where
  wireSize ft' self'@(ChatMessage x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeOpt 1 13 x'2 + P'.wireSizeOpt 1 3 x'3)
  wirePut ft' self'@(ChatMessage x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 9 x'1
             P'.wirePutOpt 16 13 x'2
             P'.wirePutOpt 24 3 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{message_text = new'Field}) (P'.wireGet 9)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{message_id = Prelude'.Just new'Field}) (P'.wireGet 13)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{time_delta = Prelude'.Just new'Field}) (P'.wireGet 3)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ChatMessage) ChatMessage where
  getVal m' f' = f' m'
 
instance P'.GPB ChatMessage
 
instance P'.ReflectDescriptor ChatMessage where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10]) (P'.fromDistinctAscList [10, 16, 24])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.Chat.ChatMessage\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Chat\"], baseName = MName \"ChatMessage\"}, descFilePath = [\"Protocol\",\"Data\",\"Chat\",\"ChatMessage.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Chat.ChatMessage.message_text\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Chat\",MName \"ChatMessage\"], baseName' = FName \"message_text\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Chat.ChatMessage.message_id\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Chat\",MName \"ChatMessage\"], baseName' = FName \"message_id\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Chat.ChatMessage.time_delta\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Chat\",MName \"ChatMessage\"], baseName' = FName \"time_delta\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ChatMessage where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ChatMessage where
  textPut msg
   = do
       P'.tellT "message_text" (message_text msg)
       P'.tellT "message_id" (message_id msg)
       P'.tellT "time_delta" (time_delta msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'message_text, parse'message_id, parse'time_delta]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'message_text
         = P'.try
            (do
               v <- P'.getT "message_text"
               Prelude'.return (\ o -> o{message_text = v}))
        parse'message_id
         = P'.try
            (do
               v <- P'.getT "message_id"
               Prelude'.return (\ o -> o{message_id = v}))
        parse'time_delta
         = P'.try
            (do
               v <- P'.getT "time_delta"
               Prelude'.return (\ o -> o{time_delta = v}))