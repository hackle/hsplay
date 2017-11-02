module Codewars.Kata.SqIntoSq where

import Data.List
import Control.Applicative
    
decompose :: Integer -> Maybe [Integer]
decompose n = roots $ seek (n ^ 2) [] squares where
    squares = reverse $ (^2) <$> [1..(n-1)]
    roots :: Maybe [Integer] -> Maybe [Integer]
    roots = (((root) <$>) <$>)

root :: Integer -> Integer
root = round . sqrt . fromInteger

seek :: Integer -> [Integer] -> [Integer] -> Maybe [Integer]
seek tot taken rest = 
    let sumTaken = sum taken
        sumRest = sum rest
        diff = tot - sumTaken
        stillPossible = sumTaken < tot && sumRest >= diff
        in case (sumTaken == tot, stillPossible, rest) of
            (True, _, _) -> Just taken
            (_, False, _) -> Nothing
            (False, _, []) -> Nothing
            (False, _, (x:xs)) -> 
                let diff' = diff - x
                    next = seek tot (x:taken) (dropWhile (\n -> n > diff') xs)
                    in next <|> seek tot taken (dropWhile (\n -> n > diff) xs)