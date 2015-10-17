{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.ContactRequest.Response.Status (Status(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Status = Undefined
            | Pending
            | Accepted
            | Rejected
            | Error
            deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Status
 
instance Prelude'.Bounded Status where
  minBound = Undefined
  maxBound = Error
 
instance P'.Default Status where
  defaultValue = Undefined
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Status
toMaybe'Enum 0 = Prelude'.Just Undefined
toMaybe'Enum 1 = Prelude'.Just Pending
toMaybe'Enum 2 = Prelude'.Just Accepted
toMaybe'Enum 3 = Prelude'.Just Rejected
toMaybe'Enum 4 = Prelude'.Just Error
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Status where
  fromEnum Undefined = 0
  fromEnum Pending = 1
  fromEnum Accepted = 2
  fromEnum Rejected = 3
  fromEnum Error = 4
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Protocol.Data.ContactRequest.Response.Status") .
      toMaybe'Enum
  succ Undefined = Pending
  succ Pending = Accepted
  succ Accepted = Rejected
  succ Rejected = Error
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Protocol.Data.ContactRequest.Response.Status"
  pred Pending = Undefined
  pred Accepted = Pending
  pred Rejected = Accepted
  pred Error = Rejected
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Protocol.Data.ContactRequest.Response.Status"
 
instance P'.Wire Status where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Status
 
instance P'.MessageAPI msg' (msg' -> Status) Status where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Status where
  reflectEnum
   = [(0, "Undefined", Undefined), (1, "Pending", Pending), (2, "Accepted", Accepted), (3, "Rejected", Rejected),
      (4, "Error", Error)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Protocol.Data.ContactRequest.Response.Status") [] ["Protocol", "Data", "ContactRequest", "Response"]
        "Status")
      ["Protocol", "Data", "ContactRequest", "Response", "Status.hs"]
      [(0, "Undefined"), (1, "Pending"), (2, "Accepted"), (3, "Rejected"), (4, "Error")]
 
instance P'.TextType Status where
  tellT = P'.tellShow
  getT = P'.getRead