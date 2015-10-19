import Distribution.Simple
import Data.Monoid((<>))

uncurry2 :: (((a, b) -> c) -> ((a1, b1) -> c1) -> d) -> ((a -> b -> c) -> (a1 -> b1 -> c1) -> d)
uncurry2 = flip (.) uncurry . flip . flip (.) uncurry . flip

combine' :: (Monad m, Monoid b) => (a -> m b) -> (a -> m b) -> (a -> m b)
combine' f g x = f x >>= \rf -> g x >>= \rg -> return $ rf <> rg

combine :: (Monad m, Monoid c) => (a -> b -> m c) -> (a -> b -> m c) -> (a -> b -> m c)
combine = fmap (fmap curry) $ uncurry2 combine'

runHprotoc = return () -- TODO: Run hprotoc
preConfHook = preConf simpleUserHooks `combine` const (const (runHprotoc >> return mempty))
main = defaultMainWithHooks $ simpleUserHooks { preConf = preConfHook }
