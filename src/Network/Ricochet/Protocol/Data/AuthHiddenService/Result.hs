{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.AuthHiddenService.Result (Result(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Result = Result{accepted :: !(P'.Bool), is_known_contact :: !(P'.Maybe P'.Bool)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Result where
  mergeAppend (Result x'1 x'2) (Result y'1 y'2) = Result (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default Result where
  defaultValue = Result P'.defaultValue P'.defaultValue
 
instance P'.Wire Result where
  wireSize ft' self'@(Result x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 8 x'1 + P'.wireSizeOpt 1 8 x'2)
  wirePut ft' self'@(Result x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 8 x'1
             P'.wirePutOpt 16 8 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{accepted = new'Field}) (P'.wireGet 8)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{is_known_contact = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Result) Result where
  getVal m' f' = f' m'
 
instance P'.GPB Result
 
instance P'.ReflectDescriptor Result where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8]) (P'.fromDistinctAscList [8, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.AuthHiddenService.Result\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\"], baseName = MName \"Result\"}, descFilePath = [\"Protocol\",\"Data\",\"AuthHiddenService\",\"Result.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.AuthHiddenService.Result.accepted\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\",MName \"Result\"], baseName' = FName \"accepted\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.AuthHiddenService.Result.is_known_contact\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\",MName \"Result\"], baseName' = FName \"is_known_contact\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType Result where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg Result where
  textPut msg
   = do
       P'.tellT "accepted" (accepted msg)
       P'.tellT "is_known_contact" (is_known_contact msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'accepted, parse'is_known_contact]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'accepted
         = P'.try
            (do
               v <- P'.getT "accepted"
               Prelude'.return (\ o -> o{accepted = v}))
        parse'is_known_contact
         = P'.try
            (do
               v <- P'.getT "is_known_contact"
               Prelude'.return (\ o -> o{is_known_contact = v}))