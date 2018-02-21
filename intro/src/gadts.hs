{-# LANGUAGE GADTs, StandaloneDeriving #-}

data Expr a where
    I :: Int -> Expr Int
    B :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq :: Eq a => Expr a -> Expr a -> Expr Bool
    
eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add ex1 ex2) = eval ex1 + eval ex2
eval (Mul ex1 ex2) = eval ex1 * eval ex2 
eval (Eq ex1 ex2) = eval ex1 == eval ex2