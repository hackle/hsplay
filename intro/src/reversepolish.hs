module RPN where

import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Data.List.Split (splitOn)
import Data.List

type Parser = Parsec String ()
    
calc :: String -> Double
calc str = eval $ either (const (Lit 0)) id (parse (exps <|> lit) "" (rev str))

rev :: String -> String
rev = (intercalate " ") . reverse . (splitOn " ")

data Expr = 
    Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Lit Double deriving (Show, Eq)

eval :: Expr -> Double
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval b - eval a
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval b / eval a
eval (Lit a) = a

exps :: Parser Expr
exps = do
    spaces
    f <- op
    spaces
    a <- term
    spaces
    b <- term
    spaces
    return (f a b)

term :: Parser Expr
term = lit <|> exps

lit :: Parser Expr
lit = Lit <$> double

op :: Parser (Expr -> Expr -> Expr)
op = do { o <- oneOf opChars; return (getOp o) }

opChars :: [Char]
opChars = "+-*/"

getOp :: Char -> (Expr -> Expr -> Expr)
getOp c = snd $ head $ filter ((c ==).fst) (zip opChars [Add, Sub, Mul, Div])

double :: Parser Double
double = do
    w <- many1 digit
    d <- decimal <|> pure []
    return $ read (w++d)

decimal :: Parser [Char]
decimal = do 
    char '.'
    d <- many1 digit
    return $ '.':d

