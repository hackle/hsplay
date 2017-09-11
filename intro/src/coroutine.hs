import Control.Monad
import Control.Monad.Trans.Class

data PauseT m r = RunT (m (PauseT m r)) | DoneT r

-- show implement these
instance (Monad m) => Monad (PauseT m) where
-- return :: Monad m => a -> PauseT m a
return a = DoneT a

-- (>>=) :: Monad m => PauseT m a -> (a -> PauseT m b) -> PauseT m b
DoneT r >>= f = DoneT r
RunT m >>= f = RunT $ liftM (>>= f) m

-- instance MonadTrans PauseT where
-- -- lift :: Monad m => m a -> PauseT m a
-- lift m = undefined

-- pause :: Monad m => PauseT m ()
-- pause = undefined


-- -- bonus exercise, implement joinP
-- -- double bonus: without relying on PauseT's monad instance
-- -- triple bonus: explain in English what joinP *means* for the Pause monad
-- joinP :: Monad m => PauseT m (PauseT m a) -> PauseT m a
-- joinP = undefined

-- -- show ...and see if it compiles.
-- main = putStrLn "it compiles"