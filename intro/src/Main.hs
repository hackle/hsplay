import Data.List
import Control.Applicative
import Data.Function (fix)

main = do
    putStrLn $ show (processingRequests 2 [1, 5, 6] == 1)
    putStrLn $ show (processingRequests 3 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] == 3)
    putStrLn $ show (processingRequests 10 [1, 2, 3, 5, 7, 11, 13, 15, 19, 22, 27, 30, 2000] == 9)
    putStrLn $ show (processingRequests 5 [1, 2, 3] == 3)
    putStrLn $ show (processingRequests 0 [1, 2, 3] == 0)
    putStrLn $ show (processingRequests 3 [10, 5, 6] == 1)

processingRequests :: Int -> [Int] -> Int
processingRequests tot reqs =
    fst $ foldl go (0, reqs) ranges
    where
        go :: (Int, [Int]) -> (Int -> Bool) -> (Int, [Int])
        go (idx, reqs) inRange = 
            case break inRange reqs of
            (xs, [])    -> (idx, reqs)
            (xs, y:ys)  -> (idx + 1, xs++ys)
        ranges = inRange <$> [1..tot]
        inRange n = \d -> d >= n && d <= 2*n

findPair :: Int -> [Int] -> Maybe [Int]
findPair y xs@(_:_:_) = findPair' xs
    where 
        findPair' = fix $ \rec xs -> do
            xs' <- find eqY [ z | y <- tails xs, z <- inits y ]
            rec (init $ tail xs') <|> Just xs'
        eqY ys@(_:_:_) = head ys + last ys == y
        eqY _ = False
findPair _ _ = Nothing