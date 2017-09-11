{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
import Control.Monad (liftM )
import Control.Monad.Trans (MonadTrans (..))

newtype Trampoline m r = Trampoline { bounce :: m (Either (Trampoline m r ) r ) }

instance Monad m => Functor (Trampoline m) where
    -- fmap :: (a -> b) -> Trampoline m a -> Trampoline m b
    -- m (Either (Trampoline m r) r) >>= Either (Trampoline m r) r -> m Either (Trampoline m b) b
    fmap f ta = Trampoline $ bounce ta >>= either (bounce . fmap f) (return . Right . f)

instance Monad m => Applicative (Trampoline m) where
    pure = Trampoline . return . Right
    -- Trampoline m (Either (Trampoline (a -> b)) (a -> b)) -> Trampoline m (Either (Trampoline m a) a) 
    --      -> Trampoline (Either (Trampoline m b) b)
    tmf <*> tma = Trampoline $ do
        bounce tmf >>= either (bounce . (<*> tma)) (bounce . (<$> tma))

instance Monad m => Monad (Trampoline m) where
    return = Trampoline . return . Right
    t >>= f = Trampoline (bounce t >>= either (return . Left . (>>=f )) (bounce . f ))

instance MonadTrans Trampoline where
    lift = Trampoline . liftM Right

pause :: Monad m  => Trampoline m ()
pause = Trampoline (return $ Left $ return ())

run :: Monad m  => Trampoline m r -> m r
run t = bounce t >>= either run return