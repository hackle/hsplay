{-# LANGUAGE PackageImports #-}
import "matrix" Data.Matrix hiding ((<|>))
import Data.List
import Control.Applicative

-- shortest :: Matrix Int -> [Int]
-- shortest = head . (sortOn sum) . paths

-- paths :: Matrix Int -> Maybe [[Int]]

paths :: Int -> Int -> Matrix Int -> Maybe [[Int]]
paths x y mx = nexts <|> Just [[]] where
    nexts = do
        v <- safeGet x y mx
        ys <- paths x (y+1) mx
        xs <- paths (x+1) y mx
        return (((v :) <$> ys) ++ ((v :) <$> xs))

-- test
sample = fromLists [[ 5..7], [1, 3, 3], [5, 0, 2]]
result = paths 1 1 sample
