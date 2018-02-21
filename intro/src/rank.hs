{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

addRankN :: (forall n. Num n => n -> n) -> (Int, Double)
addRankN f = (f 1, f 1.0)

data Showable = forall a. Show a => Sh a
miList ::[ Showable ]
miList = [ Sh 'a' ]

instance Show Showable where
    show (Sh s) = show s

    
data Showable1 a = Show a => Sh1 a
miList1 ::[ Showable1 Char ]
miList1 = [ Sh1 'a' ]

instance Show (Showable1 a) where
    show (Sh1 s) = show s

withEffect :: Int
withEffect = 
    do putStr "blah";
    1