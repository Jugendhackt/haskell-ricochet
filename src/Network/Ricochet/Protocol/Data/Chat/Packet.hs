{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.Chat.Packet (Packet(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Protocol.Data.Chat.ChatAcknowledge as Protocol.Data.Chat (ChatAcknowledge)
import qualified Protocol.Data.Chat.ChatMessage as Protocol.Data.Chat (ChatMessage)
 
data Packet = Packet{chat_message :: !(P'.Maybe Protocol.Data.Chat.ChatMessage),
                     chat_acknowledge :: !(P'.Maybe Protocol.Data.Chat.ChatAcknowledge)}
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
             10 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{chat_message = P'.mergeAppend (chat_message old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{chat_acknowledge = P'.mergeAppend (chat_acknowledge old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Packet) Packet where
  getVal m' f' = f' m'
 
instance P'.GPB Packet
 
instance P'.ReflectDescriptor Packet where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.Data.Chat.Packet\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Chat\"], baseName = MName \"Packet\"}, descFilePath = [\"Protocol\",\"Data\",\"Chat\",\"Packet.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Chat.Packet.chat_message\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Chat\",MName \"Packet\"], baseName' = FName \"chat_message\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.Chat.ChatMessage\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Chat\"], baseName = MName \"ChatMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.Data.Chat.Packet.chat_acknowledge\", haskellPrefix' = [], parentModule' = [MName \"Protocol\",MName \"Data\",MName \"Chat\",MName \"Packet\"], baseName' = FName \"chat_acknowledge\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.Data.Chat.ChatAcknowledge\", haskellPrefix = [], parentModule = [MName \"Protocol\",MName \"Data\",MName \"Chat\"], baseName = MName \"ChatAcknowledge\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType Packet where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg Packet where
  textPut msg
   = do
       P'.tellT "chat_message" (chat_message msg)
       P'.tellT "chat_acknowledge" (chat_acknowledge msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'chat_message, parse'chat_acknowledge]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'chat_message
         = P'.try
            (do
               v <- P'.getT "chat_message"
               Prelude'.return (\ o -> o{chat_message = v}))
        parse'chat_acknowledge
         = P'.try
            (do
               v <- P'.getT "chat_acknowledge"
               Prelude'.return (\ o -> o{chat_acknowledge = v}))