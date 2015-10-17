{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.ContactRequest.Limits (Limits(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Limits = MessageMaxCharacters
            | NicknameMaxCharacters
            deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Limits
 
instance Prelude'.Bounded Limits where
  minBound = MessageMaxCharacters
  maxBound = NicknameMaxCharacters
 
instance P'.Default Limits where
  defaultValue = MessageMaxCharacters
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Limits
toMaybe'Enum 2000 = Prelude'.Just MessageMaxCharacters
toMaybe'Enum 30 = Prelude'.Just NicknameMaxCharacters
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Limits where
  fromEnum MessageMaxCharacters = 2000
  fromEnum NicknameMaxCharacters = 30
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Protocol.Data.ContactRequest.Limits") .
      toMaybe'Enum
  succ MessageMaxCharacters = NicknameMaxCharacters
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Protocol.Data.ContactRequest.Limits"
  pred NicknameMaxCharacters = MessageMaxCharacters
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Protocol.Data.ContactRequest.Limits"
 
instance P'.Wire Limits where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Limits
 
instance P'.MessageAPI msg' (msg' -> Limits) Limits where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Limits where
  reflectEnum = [(2000, "MessageMaxCharacters", MessageMaxCharacters), (30, "NicknameMaxCharacters", NicknameMaxCharacters)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Protocol.Data.ContactRequest.Limits") [] ["Protocol", "Data", "ContactRequest"] "Limits")
      ["Protocol", "Data", "ContactRequest", "Limits.hs"]
      [(2000, "MessageMaxCharacters"), (30, "NicknameMaxCharacters")]
 
instance P'.TextType Limits where
  tellT = P'.tellShow
  getT = P'.getRead