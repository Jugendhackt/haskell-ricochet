{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.Control.FeaturesEnabled (FeaturesEnabled(..)) where
import Prelude ((+), (/), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data FeaturesEnabled = FeaturesEnabled{feature :: !(P'.Seq P'.Utf8), ext'field :: !(P'.ExtField)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.ExtendMessage FeaturesEnabled where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.Mergeable FeaturesEnabled where
  mergeAppend (FeaturesEnabled x'1 x'2) (FeaturesEnabled y'1 y'2)
   = FeaturesEnabled (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default FeaturesEnabled where
  defaultValue = FeaturesEnabled P'.defaultValue P'.defaultValue
 
instance P'.Wire FeaturesEnabled where
  wireSize ft' self'@(FeaturesEnabled x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 9 x'1 + P'.wireSizeExtField x'2)
  wirePut ft' self'@(FeaturesEnabled x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 9 x'1
             P'.wirePutExtField x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{feature = P'.append (feature old'Self) new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [100 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> FeaturesEnabled) FeaturesEnabled where
  getVal m' f' = f' m'
 
instance P'.GPB FeaturesEnabled
 
instance P'.ReflectDescriptor FeaturesEnabled where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.Control.FeaturesEnabled\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Control\"], baseName = MName \"FeaturesEnabled\"}, descFilePath = [\"Protocol\",\"Data\",\"Control\",\"FeaturesEnabled.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Control.FeaturesEnabled.feature\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Control\",MName \"FeaturesEnabled\"], baseName' = FName \"feature\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 100},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType FeaturesEnabled where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg FeaturesEnabled where
  textPut msg
   = do
       P'.tellT "feature" (feature msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'feature]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'feature
         = P'.try
            (do
               v <- P'.getT "feature"
               Prelude'.return (\ o -> o{feature = P'.append (feature o) v}))