{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- import Data.Either

-- class Eq e => Collection c e | c -> e where
--     insert :: c -> e -> c
--     member :: c -> e -> Bool

-- instance Eq a => Collection [a] a where
--     insert = flip (:)
--     member = flip elem

-- class EitherSub a b c | a -> b, a -> c where
--     eLeft :: a -> b
--     eRight :: a -> c

-- class (b, c, Container b c) => EitherSub b c where
--     data Container b c :: *
--     eLeft :: Container b c -> b
--     eRight :: Container b c -> c

-- instance EitherSub ((,) a b) a b where
--     eLeft = fst
--     eRight = snd

data Nat = Zero | Succ Nat
data Vector (n::Nat) a where
    VNil :: Vector Zero a
    VCons :: a -> Vector n a -> Vector (Succ n) a

instance Show a => Show (Vector n a) where
    show VNil         = "VNil"
    show (VCons a as) = "VCons " ++ (show $ (len' as) + 1) where
        len' :: Vector n a -> Int
        len' VNil = 0
        len' (VCons x xs) = 1 + (len' xs)

type family Add n m where
    Add 'Zero n = n
    Add ('Succ n) m = 'Succ (Add n m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil rest = rest
-- append (VCons a rest) xs = append rest (VCons a xs)
append (VCons a rest) xs = VCons a (append rest xs)
