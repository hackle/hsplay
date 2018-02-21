{-# LANGUAGE PackageImports #-}
import "matrix" Data.Matrix hiding ((<|>))
import Data.List
import Control.Applicative

-- shortest :: Matrix Int -> [Int]
-- shortest = head . (sortOn sum) . paths

-- paths :: Matrix Int -> Maybe [[Int]]
paths = downs 1 1

downs :: Int -> Int -> Matrix Int -> Maybe [[Int]]
downs x y mx = nexts <|> Just [[]] where
    nexts = do
        v <- safeGet x y mx
        ds <- downs x (y+1) mx
        return ((v :) <$> ds)

-- test
sample = fromLists [[ 5..7], [1, 3, 3], [5, 0, 2]]
result = downs 1 1 sample
