import           Distribution.Simple

import           Control.Monad               (liftM2)
import           Control.Monad.Reader        (ReaderT(..))
import           Data.Function               (on)
import           System.Process              (callProcess)

runHprotoc = callProcess "hprotoc"
  [ "--haskell_out=src"
  , "--proto_path=src/Network/Ricochet/Protocol/proto"
  , "--prefix=Network.Ricochet"
  , "--lenses"
  , "AuthHiddenService.proto"
  , "ChatChannel.proto"
  , "ContactRequestChannel.proto"
  , "ControlChannel.proto"
  ]

preConfHook = preConf simpleUserHooks `combine` const (const (runHprotoc >> return mempty))
main = defaultMainWithHooks $ simpleUserHooks { preConf = preConfHook }

combine :: (Monad m, Monoid c) => (a -> b -> m c) -> (a -> b -> m c) -> (a -> b -> m c)
combine = curry ... combine' `on` uncurry

combine' :: (Monad m, Monoid b) => (a -> m b) -> (a -> m b) -> (a -> m b)
combine' = runReaderT ... liftM2 mappend `on` ReaderT

infixr 9 ...
(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(...) = (.) . (.)
