{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving, FlexibleInstances, UndecidableInstances #-}

data JobDescription = JobOne | JobTwo | JobThree

deriving instance Show JobDescription

data SJD :: JobDescription -> * where
    SJobOne :: { n1:: Int } -> SJD JobOne
    SJobTwo :: SJD JobTwo
    SJobThree :: { n3:: Int } -> SJD JobThree

deriving instance Show (SJD j)

class WithN a where
    n :: a -> Int

instance WithN (SJD JobOne) where
    n (SJobOne n1) = n1

instance WithN (SJD JobThree) where
    n (SJobThree n3) = n3

taskOneWorker :: (SJD JobOne) -> IO ()
taskOneWorker t = do
    putStrLn $ "n: " ++ (show t)

instance WithN Int where
    n = id

taskWithN :: (WithN j, j ~ SJD a) => j -> IO ()
taskWithN j = putStrLn $ show (n j)