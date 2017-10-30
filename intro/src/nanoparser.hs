module NanoParser where
import Data.Char
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Bifunctor

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser p s = case parse p s of
    [(a, [])] -> a
    [(_, _)] -> error "Not complete"
    _ -> error "Did not work"

item :: Parser Char
item = Parser $ \s -> case s of
    [] -> []
    (x:xs) -> [(x, xs)]

instance Functor Parser where
    -- (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \s -> first f <$> parse p s

instance Applicative Parser where
    -- Parser (a -> b) -> Parser a -> Parser b
    pure a = Parser $ \s -> [(a, s)]
    (Parser pf) <*> (Parser p) = Parser $ \s -> [ (f a, s2) | (f, s1) <- pf s, (a, s2) <- p s1 ]

instance Monad Parser where
    return = pure
    -- Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $ \s -> concatMap (\(a, s1) -> parse (f a) s1) (p s)

instance Alternative Parser where
    empty = mzero
    (Parser p1) <|> (Parser p2) =
        Parser $ \s -> case (p1 s, p2 s) of
            ([], xs) -> xs
            (xs, _) -> xs

instance MonadPlus Parser where
    mzero = Parser $ \_ -> []
    mplus (Parser p1) (Parser p2) = Parser $ \s -> (p1 s) ++ (p2 s)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    a <- item
    if p a then return a else empty

oneOf :: String -> Parser Char
oneOf str = satisfy $ \c -> elem c str

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p1 pf a = (p1 `recurseP` pf) <|> return a

recurseP :: Parser a -> Parser (a -> a -> a) -> Parser a
recurseP pa pf = do { a <- pa; rest a } where
    rest a = (do
        f <- pf
        b <- pa
        rest (f a b)) <|> return a

rev :: Parser (Char -> [Char])
rev = (\a -> (: [a])) <$> item
    --Parser $ \(x:xs) -> [(\c -> [c, x], xs)]

double :: Parser [Char]
double = replicate 2 <$> item

manyV :: Parser Char -> Parser [Char]
manyV v = someV v <|> pure []
-- fail <|> pure [] = pure []

someV :: Parser Char -> Parser [Char]
someV v = (:) <$> v <*> manyV v
-- fail <*> _ = fail

char :: Char -> Parser Char
char c = satisfy (== c)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
-- string (c:cs) = char c >> string cs >> return (c:cs)
-- string xs = sequence (char <$> xs)
string xs = mapM_ char xs >> return xs

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved s = token (string s)

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
    s <- string "-" <|> pure []
    d <- some digit
    read <$> return (s ++ d)

parens :: Parser a -> Parser a
parens p = do
    reserved "("
    n <- p
    reserved ")"
    return n

data Expr = 
    Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Lit Int
    deriving Show

eval :: Expr -> Int
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Sub a b) = eval a - eval b
eval (Lit a) = a

int :: Parser Expr
int  = Lit <$> number

expr :: Parser Expr
expr = ((int <|> parens expr) `recurseP` mulOp) `recurseP` addOp

-- term :: Parser Expr
-- term = factor 

-- factor :: Parser Expr
-- factor = 

infixOp :: String -> (a -> a -> a) -> Parser (a -> a ->a)
infixOp x f = do { reserved x; return f }

addOp :: Parser (Expr -> Expr -> Expr)
addOp = infixOp "+" Add <|> infixOp "-" Sub

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = infixOp "*" Mul

run :: String -> Expr
run = runParser expr