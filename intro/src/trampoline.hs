{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
import Control.Monad (liftM, liftM2, ap)
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor.Identity

newtype Coroutine s m r = Coroutine { resume :: m (Either (s (Coroutine s m r )) r ) }

instance (Monad m, Functor s) => Functor (Coroutine s m) where
    -- (a -> b) -> Coroutine s m a -> Coroutine s m b 
    fmap f ma = Coroutine $ resume ma >>= either (return . Left . (fmap $ fmap f)) (return . Right . f)

instance (Monad m, Functor s) => Applicative (Coroutine s m) where
    -- f (a -> b) -> f a -> f b
    -- Coroutine s m (a -> b) -> Coroutine s m a -> Coroutine s m b
    -- fa <*> ma = do { f <- fa; a <- ma; return $ f a; }
    pure = Coroutine . return . Right 
    Coroutine fa <*> ma = Coroutine $ fa >>= either (return . Left . (fmap (<*> ma))) (resume . (<$> ma))

instance (Functor s, Monad m) => Monad (Coroutine s m) where
    return x = Coroutine (return (Right x))
    t >>= f = Coroutine (resume t >>= either (return . Left . fmap (>>=f )) (resume . f ))
    
instance Functor s => MonadTrans (Coroutine s) where
    lift = Coroutine . liftM Right
    
suspend :: (Monad m, Functor s) => s (Coroutine s m x ) -> Coroutine s m x
suspend s = Coroutine (return (Left s))

type Trampoline m x = Coroutine Identity m x

pause :: Monad m => Trampoline m ()
pause = suspend $ Identity $ return ()

run :: Monad m => Trampoline m x -> m x
run mx = resume mx >>= either (run . runIdentity) return

hello :: Trampoline IO ()
hello = do
    lift $ putStrLn "hello"
    pause
    lift $ putStrLn "world"

instance Show (Coroutine s m x) where
    show x = "Coroutine"

runOnce :: Monad m => Trampoline m x -> Trampoline m x
runOnce mx = Coroutine $ resume mx >>= either (resume . runIdentity) (return . Right)

data EitherFunctor l r c = LeftF (l c) | RightF (r c)
instance (Functor a, Functor b) => Functor (EitherFunctor a b) where
    fmap f (LeftF l) = LeftF $ f <$> l
    fmap f (RightF r) = RightF $ f <$> r

type Transducer a b m x = Coroutine (EitherFunctor ((->) (Maybe a)) ((,) b)) m x

awaitT :: (Monad m) => Transducer a b m (Maybe a)
-- Coroutine m (Either (Maybe a -> Coroutine s m x) Maybe a)
-- suspend :: (Monad m, Functor s) => s (Coroutine s m x) -> Coroutine s m x
-- LeftF :: s (Coroutine s m x)
-- LeftF :: (Maybe a -> Coroutine s m x)
-- LeftF :: (Maybe a -> Coroutine s m (Maybe a))
awaitT = suspend $ LeftF return

yieldT :: (Monad m) => b -> Transducer a b m ()
-- Coroutine m (Left (b, Coroutine s m x))
yieldT b = suspend $ RightF (b, return ())

lift121 :: Monad m => (a -> b) -> Transducer a b m ()
lift121 f = awaitT >>= maybe (return ()) (\a -> yieldT (f a) >> lift121 f)

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do { a <- ma; b <- mb; f a b }

type EitherTransducer a b m x = Either (EitherFunctor ((->) (Maybe a)) ((,) b) (Transducer a b m x)) x

(=>=) :: Monad m => Transducer a b m x -> Transducer b c m y -> Transducer a c m (x, y)
t1 =>= t2 = Coroutine (bindM2 proceed (resume t1) (resume t2)) where
    proceed :: Monad m => EitherTransducer a b m x -> EitherTransducer b c m y -> m (EitherTransducer a c m (x, y))
    -- s :: a -> Transducer a b m x
    proceed (Right x) (Right y) = return $ Right (x, y)
    -- c :: Transducer a b m x
    -- f :: Maybe a -> Transducer b c m x
    proceed (Left (RightF (a, c))) (Left (LeftF f)) = resume $ c =>= (f $ Just a)    
    proceed (Left (RightF (a, c))) (Right r) = resume $ c =>= (return r)
    proceed (Left (LeftF f)) r = return $ Left $ LeftF $ (=>= Coroutine (return r)) . f
    proceed (Right x) (Left (LeftF f)) = resume $ return x =>= f Nothing
    proceed l (Left (RightF (a, c))) = return $ Left $ RightF (a, Coroutine (return l) =>= c)