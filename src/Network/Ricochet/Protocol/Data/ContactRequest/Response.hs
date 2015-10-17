{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.ContactRequest.Response (Response(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Protocol.Data.ContactRequest.Response.Status as Protocol.Data.ContactRequest.Response (Status)
 
data Response = Response{status :: !(Protocol.Data.ContactRequest.Response.Status)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Response where
  mergeAppend (Response x'1) (Response y'1) = Response (P'.mergeAppend x'1 y'1)
 
instance P'.Default Response where
  defaultValue = Response P'.defaultValue
 
instance P'.Wire Response where
  wireSize ft' self'@(Response x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 14 x'1)
  wirePut ft' self'@(Response x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 14 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{status = new'Field}) (P'.wireGet 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Response) Response where
  getVal m' f' = f' m'
 
instance P'.GPB Response
 
instance P'.ReflectDescriptor Response where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8]) (P'.fromDistinctAscList [8])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.ContactRequest.Response\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"ContactRequest\"], baseName = MName \"Response\"}, descFilePath = [\"Protocol\",\"Data\",\"ContactRequest\",\"Response.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.ContactRequest.Response.status\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"ContactRequest\",MName \"Response\"], baseName' = FName \"status\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.ContactRequest.Response.Status\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"ContactRequest\",MName \"Response\"], baseName = MName \"Status\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType Response where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg Response where
  textPut msg
   = do
       P'.tellT "status" (status msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'status]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'status
         = P'.try
            (do
               v <- P'.getT "status"
               Prelude'.return (\ o -> o{status = v}))