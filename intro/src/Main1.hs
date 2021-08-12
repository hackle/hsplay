import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Cont
import Control.Monad
import Data.Char
import Control.Monad.Cont
-- import FixIt

kn :: Cont r Int
kn = (`runCont` return) $ callCC $ \k -> do
    k 5
    return 25

tryCont :: MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
tryCont c h = callCC $ \ok -> do
    err <- callCC $ \notOk -> do
        x <- c notOk
        ok x
    h err

data SqrtException = LessThanZero deriving (Show, Eq)

sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
sqrtIO throw = do 
    ln <- lift (putStr "Enter a number to sqrt: " >> readLn)
    when (ln < 0) (throw LessThanZero)
    lift $ print (sqrt ln)

main = runContT (tryCont sqrtIO (lift . print)) return

-- fun :: Int -> String
-- fun n = (`runCont` id) $ do
--     str <- callCC $ \exit1 -> do                            -- define "exit1"
--         when (n < 10) (exit1 (show n))
--         let ns = map digitToInt (show (n `div` 2))
--         n' <- callCC $ \exit2 -> do                         -- define "exit2"
--             when ((length ns) < 3) (exit2 (length ns))
--             when ((length ns) < 5) (exit2 n)
--             when ((length ns) < 7) $ do
--                 let ns' = map intToDigit (reverse ns)
--                 exit1 (dropWhile (=='0') ns')               --escape 2 levels
--             return $ sum ns
--         return $ "inner: (ns = " ++ (show ns) ++ ") " ++ (show n')
--         return "inne: blah"
--     return $ "outer: Answer: " ++ str

-- kn n = (`runCont` id) $ do
--     callCC $ \k1 -> do
--         when (n < 10) $ k1 ((show n) ++ "< 10")
--         callCC $ \k2 -> do
--             when (n < 20) $ k1 ((show n) ++ "< 20")
--             when (n < 30) $ k2 ((show n) ++ "< 30")
--             return $ ((show n) ++ " negated")
--         return ((show (100 + n)) ++ " 0000")

-- -- bind::ma >>= h = mb
-- -- ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
-- -- cont $ \k -> runCont ma $ \a -> runCont (h a) k

-- -- callCC f :: ((a -> ma) -> mb) -> ma = (a -> ((b -> r) -> r)) -> ((a -> r) -> r) -> ((a -> r) -> r)
-- -- callCC f = cont $ \g -> runCont (f $ \a -> cont \_ -> g a) g

-- callCC $ \k -> do k 5; return 25
-- cont $ \g -> runCont ((\k -> do k 5; return 25) $ \a -> cont $ \_ -> g a) g
-- cont $ \g -> runCont ((\a -> cont $ \_ -> g a) 5; return 25) g
-- cont $ \g -> runCont (cont \_ -> g 5; return 25) g
-- cont $ \g -> runCont (cont \_ -> g 5 >>= (\_ -> return 25)) g
-- cont $ \g -> runCont (cont \h -> runCont (cont \_ -> g 5) (\a -> runCont (\a -> return 25) h)) g
-- cont $ \g -> runCont (cont \_ -> g 5) g
-- cont \g -> g 5

-- callCC $ \k => do return 25; k 5
-- cont $ \g -> runCont (f $ \a -> con \_ -> g a) g
-- cont $ \g -> runCont ((\k -> do return 25; k 5) $ \a -> con \_ -> g a) g
-- cont $ \g -> runCont (\k -> (return 25 >>= \_ -> k 5) $ \a -> con \_ -> g a) g
-- cont $ \g -> runCont (\k -> (\h -> runCont (return 25) (\b -> runCont ((\_ -> k 5) b) h)) $ \a -> con \_ -> g a) g
-- cont $ \g -> runCont (\k -> (\h -> runCont (return 25) (\b -> runCont (k 5) h)) $ \a -> con \_ -> g a) g
-- cont $ \g -> runCont (\k -> (\h -> runCont (cont \j -> j 25) (\b -> runCont (k 5) h)) $ \a -> con \_ -> g a) g
-- cont $ \g -> runCont (\k -> (\h -> (\j -> j 25) (\b -> runCont (k 5) h)) $ \a -> con \_ -> g a) g
-- cont $ \g -> runCont (\k -> (\h -> (\b -> runCont (k 5) h) 25) $ \a -> con \_ -> g a) g
-- cont $ \g -> runCont ((\k -> (\h -> runCont (k 5) h)) (\a -> con \_ -> g a)) g
-- cont $ \g -> runCont (\h -> runCont ((\a -> con \_ -> g a) 5) h) g
-- cont $ \g -> runCont (\h -> runCont (con \_ -> g 5) h) g
-- cont $ \g -> runCont (\_ -> g 5) g
-- cont $ \g -> g 5

-- do n1 <- callCC $ \k1 -> do
--     k1 n; 
--     n2 <- callCC $ \k2 -> do
--         k2 n;
--         return "2"
--     return n2;
-- return n1;

-- callCC $ \k -> callCC $ \k1 -> k 5; return 25
-- cont \h -> runCont (cont \k -> h 5) h