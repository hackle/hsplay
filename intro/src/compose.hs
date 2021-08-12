{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleInstances #-}

data Thud x = Thud deriving (Functor, Show)

instance Applicative Thud where
    pure _ = Thud
    Thud <*> Thud = Thud

instance Monad Thud where
    return = pure
    Thud >>= f = Thud

data Flip x = Flip Bool x deriving (Functor, Show)

instance Applicative Flip where
    pure x = Flip False x
    Flip False f <*> Flip b a = Flip b (f a)
    Flip _ f <*> Flip b a = Flip (not b) (f a)

instance Monad Flip where
    return = pure
    Flip False a >>= f = f a
    Flip _ a >>= f = Flip (not b) c where Flip b c = f a

newtype (:.:) f g x = C (f (g x)) deriving (Show, Eq)

instance Functor (Flip :.: Thud) where
    fmap f (C (Flip b Thud)) = C (Flip b Thud)

instance Applicative (Flip :.: Thud) where
    pure x = C (Flip False Thud)
    C (Flip False Thud) <*> C (Flip b Thud) = C (Flip b Thud)
    _ <*> C (Flip b Thud) = C (Flip (not b) Thud)

instance Monad (Flip :.: Thud) where
    return = pure
    C (Flip b Thud) >>= _ = C (Flip b Thud)

newtype Const a b = Const a deriving (Show, Functor, Eq)

instance Monoid a => Applicative (Const a) where
    pure _ = Const mempty
    Const a <*> Const b = Const (mappend a b)

instance Functor (Const a :.: Maybe) where
    fmap f (C xs) = C $ (f <$>) <$> xs

instance Monoid a => Applicative (Const a :.: Maybe) where
    pure x = C . pure . pure $ x
    C (Const x) <*> C (Const y) = C $ Const (mappend x y)

instance Monoid a => Monad (Const a :.: Maybe) where
    return = pure
    C (Const x) >>= f = C (Const x)

-- C (Const x) >>= return 

mb :: ((Const String) :.: Maybe) String
mb = C $ Const "a"
-- return "a" >>= f 

ma :: ((Const String) :.: Maybe) String
ma = return "a"

-- app :: [Int -> (a -> b)] -> [Int -> a] -> [Int -> b]
-- app f x = let b = ((<*>) <$>) f in b <*> x

app :: (Applicative a1, Applicative a2) => a1 (a2 (a -> b)) -> a1 (a2 a) -> a1 (a2 b)
app f a = (<*>) <$> f <*> a

-- app1 :: (Int -> (a -> b)) -> (Int -> a) -> (Int -> b)
-- app1 f x = let b = f <*> x in _