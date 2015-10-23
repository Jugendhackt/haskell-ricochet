import           Distribution.Simple

import           Control.Applicative ((<$>))
import           Data.Monoid         ((<>))
import           System.Process      (callProcess)

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

uncurry2 :: (((a, b) -> c) -> ((a1, b1) -> c1) -> d) -> (a -> b -> c) -> (a1 -> b1 -> c1) -> d
uncurry2 = flip (.) uncurry . flip . flip (.) uncurry . flip

combine' :: (Monad m, Monoid b) => (a -> m b) -> (a -> m b) -> a -> m b
combine' f g x = f x >>= \rf -> g x >>= \rg -> return $ rf <> rg

combine :: (Monad m, Monoid c) => (a -> b -> m c) -> (a -> b -> m c) -> a -> b -> m c
combine = fmap curry <$> uncurry2 combine'
