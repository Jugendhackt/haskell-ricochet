{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.Chat.ChatAcknowledge (ChatAcknowledge(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ChatAcknowledge = ChatAcknowledge{message_id :: !(P'.Maybe P'.Word32), accepted :: !(P'.Maybe P'.Bool)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ChatAcknowledge where
  mergeAppend (ChatAcknowledge x'1 x'2) (ChatAcknowledge y'1 y'2)
   = ChatAcknowledge (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default ChatAcknowledge where
  defaultValue = ChatAcknowledge P'.defaultValue (Prelude'.Just Prelude'.True)
 
instance P'.Wire ChatAcknowledge where
  wireSize ft' self'@(ChatAcknowledge x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 13 x'1 + P'.wireSizeOpt 1 8 x'2)
  wirePut ft' self'@(ChatAcknowledge x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 13 x'1
             P'.wirePutOpt 16 8 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{message_id = Prelude'.Just new'Field}) (P'.wireGet 13)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{accepted = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ChatAcknowledge) ChatAcknowledge where
  getVal m' f' = f' m'
 
instance P'.GPB ChatAcknowledge
 
instance P'.ReflectDescriptor ChatAcknowledge where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.Chat.ChatAcknowledge\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Chat\"], baseName = MName \"ChatAcknowledge\"}, descFilePath = [\"Protocol\",\"Data\",\"Chat\",\"ChatAcknowledge.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Chat.ChatAcknowledge.message_id\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Chat\",MName \"ChatAcknowledge\"], baseName' = FName \"message_id\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Chat.ChatAcknowledge.accepted\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Chat\",MName \"ChatAcknowledge\"], baseName' = FName \"accepted\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"true\", hsDefault = Just (HsDef'Bool True)}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ChatAcknowledge where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ChatAcknowledge where
  textPut msg
   = do
       P'.tellT "message_id" (message_id msg)
       P'.tellT "accepted" (accepted msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'message_id, parse'accepted]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'message_id
         = P'.try
            (do
               v <- P'.getT "message_id"
               Prelude'.return (\ o -> o{message_id = v}))
        parse'accepted
         = P'.try
            (do
               v <- P'.getT "accepted"
               Prelude'.return (\ o -> o{accepted = v}))