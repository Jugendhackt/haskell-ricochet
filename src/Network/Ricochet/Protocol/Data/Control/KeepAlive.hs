{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.Control.KeepAlive (KeepAlive(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data KeepAlive = KeepAlive{response_requested :: !(P'.Bool)}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable KeepAlive where
  mergeAppend (KeepAlive x'1) (KeepAlive y'1) = KeepAlive (P'.mergeAppend x'1 y'1)
 
instance P'.Default KeepAlive where
  defaultValue = KeepAlive P'.defaultValue
 
instance P'.Wire KeepAlive where
  wireSize ft' self'@(KeepAlive x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 8 x'1)
  wirePut ft' self'@(KeepAlive x'1)
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
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{response_requested = new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> KeepAlive) KeepAlive where
  getVal m' f' = f' m'
 
instance P'.GPB KeepAlive
 
instance P'.ReflectDescriptor KeepAlive where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8]) (P'.fromDistinctAscList [8])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.Control.KeepAlive\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\"], baseName = MName \"KeepAlive\"}, descFilePath = [\"Protocol\",\"Data\",\"Control\",\"KeepAlive.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.KeepAlive.response_requested\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"KeepAlive\"], baseName' = FName \"response_requested\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType KeepAlive where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg KeepAlive where
  textPut msg
   = do
       P'.tellT "response_requested" (response_requested msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'response_requested]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'response_requested
         = P'.try
            (do
               v <- P'.getT "response_requested"
               Prelude'.return (\ o -> o{response_requested = v}))