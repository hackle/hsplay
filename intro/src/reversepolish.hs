module RPN where

import Text.Parsec
import Data.List.Split (splitOn)
import Data.List

type Parser = Parsec String ()
    
calc :: String -> Double
calc str = either (const 0) id (parse (exps <|> double) "" (rev str))

rev :: String -> String
rev = (intercalate " ") . reverse . (splitOn " ")

exps :: Parser Double
exps = do
    spaces
    f <- op
    spaces
    a <- term
    spaces
    b <- term
    spaces
    return (f a b)

term :: Parser Double
term = double <|> exps

charOpMaps :: [(Char, (Double -> Double -> Double))]
charOpMaps = [ ('+', (+)), ('-', flip (-)), ('*', (*)), ('/', flip (/))]

op :: Parser (Double -> Double -> Double)
op = do { o <- oneOf (fst <$> charOpMaps); return (getOp o) }

getOp :: Char -> (Double -> Double -> Double)
getOp c = snd $ head $ filter ((c ==).fst) charOpMaps

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

