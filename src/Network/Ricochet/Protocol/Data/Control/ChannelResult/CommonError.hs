{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Protocol.Data.Control.ChannelResult.CommonError (CommonError(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data CommonError = GenericError
                 | UnknownTypeError
                 | UnauthorizedError
                 | BadUsageError
                 | FailedError
                 deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable CommonError
 
instance Prelude'.Bounded CommonError where
  minBound = GenericError
  maxBound = FailedError
 
instance P'.Default CommonError where
  defaultValue = GenericError
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe CommonError
toMaybe'Enum 0 = Prelude'.Just GenericError
toMaybe'Enum 1 = Prelude'.Just UnknownTypeError
toMaybe'Enum 2 = Prelude'.Just UnauthorizedError
toMaybe'Enum 3 = Prelude'.Just BadUsageError
toMaybe'Enum 4 = Prelude'.Just FailedError
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum CommonError where
  fromEnum GenericError = 0
  fromEnum UnknownTypeError = 1
  fromEnum UnauthorizedError = 2
  fromEnum BadUsageError = 3
  fromEnum FailedError = 4
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Protocol.Data.Control.ChannelResult.CommonError")
      . toMaybe'Enum
  succ GenericError = UnknownTypeError
  succ UnknownTypeError = UnauthorizedError
  succ UnauthorizedError = BadUsageError
  succ BadUsageError = FailedError
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Protocol.Data.Control.ChannelResult.CommonError"
  pred UnknownTypeError = GenericError
  pred UnauthorizedError = UnknownTypeError
  pred BadUsageError = UnauthorizedError
  pred FailedError = BadUsageError
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Protocol.Data.Control.ChannelResult.CommonError"
 
instance P'.Wire CommonError where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB CommonError
 
instance P'.MessageAPI msg' (msg' -> CommonError) CommonError where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum CommonError where
  reflectEnum
   = [(0, "GenericError", GenericError), (1, "UnknownTypeError", UnknownTypeError), (2, "UnauthorizedError", UnauthorizedError),
      (3, "BadUsageError", BadUsageError), (4, "FailedError", FailedError)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Protocol.Data.Control.ChannelResult.CommonError") [] ["Protocol", "Data", "Control", "ChannelResult"]
        "CommonError")
      ["Protocol", "Data", "Control", "ChannelResult", "CommonError.hs"]
      [(0, "GenericError"), (1, "UnknownTypeError"), (2, "UnauthorizedError"), (3, "BadUsageError"), (4, "FailedError")]
 
instance P'.TextType CommonError where
  tellT = P'.tellShow
  getT = P'.getRead