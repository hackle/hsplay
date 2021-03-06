module Codewars.Kata.SqIntoSq where

import Data.List
import Control.Applicative
    
decompose :: Integer -> Maybe [Integer]
decompose n = seek tot (n-1) tot [] where tot = n ^ 2

root :: Integer -> Integer
root = floor . sqrt . fromInteger

seek :: Integer -> Integer -> Integer -> [Integer] -> Maybe [Integer]
seek _ _ 0 result = if nub result == result then Just result else Nothing
seek _ _ _ (1:_) = Nothing
seek _ 0 _ _ = Nothing
seek tot attempt remainder result = continued <|> jumped where
    continued = 
        let nextRemainder = remainder - attempt^2 
            nextRoot = root nextRemainder
        in seek tot nextRoot nextRemainder (attempt:result)
    jumped = seek tot (attempt - 1) remainder result