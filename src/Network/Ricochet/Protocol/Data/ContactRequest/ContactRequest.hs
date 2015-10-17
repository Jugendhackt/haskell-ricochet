{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.ContactRequest.ContactRequest (ContactRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ContactRequest = ContactRequest{nickname :: !(P'.Maybe P'.Utf8), message_text :: !(P'.Maybe P'.Utf8)}
                    deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ContactRequest where
  mergeAppend (ContactRequest x'1 x'2) (ContactRequest y'1 y'2) = ContactRequest (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default ContactRequest where
  defaultValue = ContactRequest P'.defaultValue P'.defaultValue
 
instance P'.Wire ContactRequest where
  wireSize ft' self'@(ContactRequest x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2)
  wirePut ft' self'@(ContactRequest x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 9 x'1
             P'.wirePutOpt 18 9 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{nickname = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{message_text = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ContactRequest) ContactRequest where
  getVal m' f' = f' m'
 
instance P'.GPB ContactRequest
 
instance P'.ReflectDescriptor ContactRequest where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.ContactRequest.ContactRequest\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"ContactRequest\"], baseName = MName \"ContactRequest\"}, descFilePath = [\"Protocol\",\"Data\",\"ContactRequest\",\"ContactRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.ContactRequest.ContactRequest.nickname\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"ContactRequest\",MName \"ContactRequest\"], baseName' = FName \"nickname\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.ContactRequest.ContactRequest.message_text\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"ContactRequest\",MName \"ContactRequest\"], baseName' = FName \"message_text\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ContactRequest where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ContactRequest where
  textPut msg
   = do
       P'.tellT "nickname" (nickname msg)
       P'.tellT "message_text" (message_text msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'nickname, parse'message_text]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'nickname
         = P'.try
            (do
               v <- P'.getT "nickname"
               Prelude'.return (\ o -> o{nickname = v}))
        parse'message_text
         = P'.try
            (do
               v <- P'.getT "message_text"
               Prelude'.return (\ o -> o{message_text = v}))