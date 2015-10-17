{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.AuthHiddenService.Proof (Proof(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Proof = Proof{public_key :: !(P'.Maybe P'.ByteString), signature :: !(P'.Maybe P'.ByteString)}
           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Proof where
  mergeAppend (Proof x'1 x'2) (Proof y'1 y'2) = Proof (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default Proof where
  defaultValue = Proof P'.defaultValue P'.defaultValue
 
instance P'.Wire Proof where
  wireSize ft' self'@(Proof x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 12 x'1 + P'.wireSizeOpt 1 12 x'2)
  wirePut ft' self'@(Proof x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 12 x'1
             P'.wirePutOpt 18 12 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{public_key = Prelude'.Just new'Field}) (P'.wireGet 12)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{signature = Prelude'.Just new'Field}) (P'.wireGet 12)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Proof) Proof where
  getVal m' f' = f' m'
 
instance P'.GPB Proof
 
instance P'.ReflectDescriptor Proof where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.AuthHiddenService.Proof\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\"], baseName = MName \"Proof\"}, descFilePath = [\"Protocol\",\"Data\",\"AuthHiddenService\",\"Proof.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.AuthHiddenService.Proof.public_key\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\",MName \"Proof\"], baseName' = FName \"public_key\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.AuthHiddenService.Proof.signature\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\",MName \"Proof\"], baseName' = FName \"signature\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType Proof where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg Proof where
  textPut msg
   = do
       P'.tellT "public_key" (public_key msg)
       P'.tellT "signature" (signature msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'public_key, parse'signature]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'public_key
         = P'.try
            (do
               v <- P'.getT "public_key"
               Prelude'.return (\ o -> o{public_key = v}))
        parse'signature
         = P'.try
            (do
               v <- P'.getT "signature"
               Prelude'.return (\ o -> o{signature = v}))