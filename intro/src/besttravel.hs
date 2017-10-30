import Data.List

-- chooseBestSum 163 3  [50, 55, 56, 57, 58] == Some 163
-- chooseBestSum 163 3 [ 50 ] == None
-- chooseBestSum 230 3 [ 91, 74, 73, 85, 73, 81, 87 ] == Some 228
-- chooseBestSum 331 2 [91,74,73,85,73,81,87] == Just 178
-- possibles :: [Int] -> [[Int]]
-- possibles [] = []
-- possibles [x] = [[x]]
-- possibles (x:xs) = (x:) <$> (concat (possibles <$> xs))

chooseBestSum :: Int -> Int -> [ Int ] -> Maybe Int
chooseBestSum limit cnt xs = find (<= limit) sorted where
    sorted = reverse $ sort sums
    sums = sum <$> filter ((cnt ==).length) (subs cnt xs)
    
subs :: Int -> [a] -> [[a]]
subs max ls = foldr (++) [] subsubs where
    subsubs = subs' max [] <$> subsRight ls

subs' :: Int -> [a] -> [a] -> [[a]]
subs' _ taken [] = [ taken ]
subs' max taken w@(x:xs) = if length next >= max then [next] else foldr (++) [] subsubs where
    next = taken++[x] 
    subsubs = subs' max next <$> subsRight xs

subsRight :: [a] -> [[a]]
subsRight [] = [[]]
subsRight w@(x:xs) = w:(subsRight xs)