data Pair a = Pair a a

alpha :: (Int -> x) -> Pair x x
alpha h = Pair (h 1) (h 2)

beta :: Pair x x -> (Int -> x)
beta (Pair a b) = Const a

alpha1 :: (Int -> x) -> Reader Bool x
alpha1 h = \t -> h 12

beta :: Reader Bool x -> (Int -> x)
beta h = \num -> h True

data Stream x = Cons x (Stream x)

class Representable f where
    type Ref f :: *
    tabulate :: (Rep f -> x) -> f x
    index :: f x -> Rep f -> x

instance Representable Stream where
    type Rep Stream = Int
    tabulate f = Cons (f 0) (tabulate (f . (+1)))