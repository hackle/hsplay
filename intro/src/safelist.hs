{-# LANGUAGE DataKinds, KindSignatures, GADTs, StandaloneDeriving #-}

data E = Empty | NonEmpty

data EList :: * -> E -> * where
    ENil :: EList a Empty
    ECons :: a -> EList a b -> EList a NonEmpty

deriving instance Show a => Show (EList a b)

safeHead :: EList a NonEmpty -> a
safeHead (x `ECons` _) = x