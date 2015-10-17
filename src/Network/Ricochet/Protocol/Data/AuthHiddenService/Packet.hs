{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.AuthHiddenService.Packet (Packet(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Protocol.Data.AuthHiddenService.Proof as Protocol.Data.AuthHiddenService (Proof)
import qualified Protocol.Data.AuthHiddenService.Result as Protocol.Data.AuthHiddenService (Result)
 
data Packet = Packet{proof :: !(P'.Maybe Protocol.Data.AuthHiddenService.Proof),
                     result :: !(P'.Maybe Protocol.Data.AuthHiddenService.Result)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Packet where
  mergeAppend (Packet x'1 x'2) (Packet y'1 y'2) = Packet (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default Packet where
  defaultValue = Packet P'.defaultValue P'.defaultValue
 
instance P'.Wire Packet where
  wireSize ft' self'@(Packet x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 11 x'2)
  wirePut ft' self'@(Packet x'1 x'2)
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
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{proof = P'.mergeAppend (proof old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{result = P'.mergeAppend (result old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Packet) Packet where
  getVal m' f' = f' m'
 
instance P'.GPB Packet
 
instance P'.ReflectDescriptor Packet where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.AuthHiddenService.Packet\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\"], baseName = MName \"Packet\"}, descFilePath = [\"Protocol\",\"Data\",\"AuthHiddenService\",\"Packet.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.AuthHiddenService.Packet.proof\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\",MName \"Packet\"], baseName' = FName \"proof\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.AuthHiddenService.Proof\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\"], baseName = MName \"Proof\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.AuthHiddenService.Packet.result\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\",MName \"Packet\"], baseName' = FName \"result\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.AuthHiddenService.Result\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"AuthHiddenService\"], baseName = MName \"Result\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType Packet where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg Packet where
  textPut msg
   = do
       P'.tellT "proof" (proof msg)
       P'.tellT "result" (result msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'proof, parse'result]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'proof
         = P'.try
            (do
               v <- P'.getT "proof"
               Prelude'.return (\ o -> o{proof = v}))
        parse'result
         = P'.try
            (do
               v <- P'.getT "result"
               Prelude'.return (\ o -> o{result = v}))