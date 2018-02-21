{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, UndecidableInstances #-}

import Data.Singletons

data Nat = Ze | Su Nat

instance Show Nat where
    show Ze = "0"
    show n = show (go n) where
        go (Su Ze) = 1
        go (Su m) = 1 + (go m)

infixl 6 :+
infixl 7 :*

type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Ze :+ m = m
type instance (Su m) :+ n = Su (m :+ n)

type family (n :: Nat) :* (m :: Nat) :: Nat
type instance Ze :* m = Ze
type instance (Su n) :* m = (n :* m) :+ m

data Vec :: * -> Nat -> * where
    Nil :: Vec a Ze
    (:-) :: a -> Vec a n -> Vec a (Su n)

infix 5 :-

instance (Show a) => Show (Vec a v) where
    show Nil = "Nil"
    show (a :- v) = " (" ++ (show a) ++ " " ++ (show v) ++ ")"

addVec :: Nat -> Nat -> Nat
addVec n Ze = n
addVec n (Su m) = Su (addVec n m)

vhead :: Vec a (Su n) -> a
vhead (a :- _) = a

vtail :: Vec a (Su n) -> Vec a n
vtail (_ :- xs) = xs

vappend :: Vec a n -> Vec a m -> Vec a (n :+ m)
vappend Nil a = a
vappend (x:-xs) a = x :- (vappend xs a)

v2List :: Vec a n -> [ a ]
v2List Nil = []
v2List (x :- xs) = x:(v2List xs)

-- vFromList :: [a] -> Vec a n
-- vFromList [] = Nil :: Vec a n
-- vFromList (x:xs) = x :- (vFromList xs)

-- emptyOrFirst :: [a] -> Vec a n
-- emptyOrFirst [] = Nil
-- emptyOrFirst (x:xs) = x:-Nil

vMap :: (a -> b) -> Vec a n -> Vec b n
vMap f Nil = Nil
vMap f (x:-xs) = (f x) :- (vMap f xs)

uncons :: Vec a (Su n) -> (a, Vec a n)
uncons (x:-xs) = (x, xs)

vInit :: Vec a (Su n) -> Vec a n
vInit (_:-Nil) = Nil
vInit (x:-(y:-xs)) = x:-(vInit (y:-xs))

zipWithSame :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWithSame _ Nil _ = Nil
zipWithSame f (x:-xs) (y:-ys) = (f x y) :- (zipWithSame f xs ys)

data SNat n where
    SZe :: SNat Ze
    SSu :: SNat n -> SNat (Su n)
    
deriving instance Show (SNat n)

instance SingI (SNat n) where
    sing 

vrep :: (SNat n) -> a -> Vec a n
vrep SZe _ = Nil
vrep (SSu n) a = a :- (vrep n a)

vFromList :: (SNat n) -> [a] -> Maybe (Vec a n)
vFromList SZe _ = Just Nil
vFromList (SSu n) (x:xs) = (x :-) <$> (vFromList n xs)
vFromList _ [] = Nothing

infixl 6 %:+
infixl 7 %:*

(%:+) :: SNat n -> SNat m -> SNat (n :+ m)
SZe %:+ m = m
(SSu n) %:+ m = SSu (n %:+ m)

(%:*) :: SNat n -> SNat m -> SNat (n :* m)
SZe %:* _ = SZe
(SSu n) %:* m = (n %:* m) %:+ m

sLength :: Vec a n -> SNat n
sLength Nil = SZe
sLength (_:-xs) = SSu (sLength xs)