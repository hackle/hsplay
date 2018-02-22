{-# LANGUAGE PackageImports #-}
import "matrix" Data.Matrix hiding ((<|>))
import Data.List
import Data.Ord
import Data.List.Split
import Control.Applicative

data Path = Path { getPos :: (Int, Int), getSum::Int } deriving Eq

shortest1 :: Matrix Int -> Int
shortest1 mx = go [ Path (1, 1) start ] start where
    start = mx!(1,1)
    (lx, ly) = (nrows mx, ncols mx)
    go :: [ Path ] -> Int -> Int
    go paths minV =
        let (minPath:xs) = paths in
            if (lx, ly) == getPos minPath
                then getSum minPath
                else 
                    let nexts = advance minPath
                        minNew = foldl min minV (getSum <$> nexts) 
                        paths' = foldl (\ps p -> insertBy comparePaths p ps) xs nexts in
                        go paths' minNew
    advance :: Path -> [Path]
    advance (Path (y, x) val) = combine right1 down1 where
        right1 = getPath (y, x+1) val
        down1 = getPath (y+1, x) val
    combine p1 p2 = let (Just xs) = sequence $ filter (\p -> p /= Nothing) [ p1, p2 ] in xs
    getPath (y, x) val = do
        v <- safeGet y x mx
        return $ Path (y, x) (val + v)
    comparePaths = comparing getSum

-- paths :: Matrix Int -> Maybe [[Int]]
-- paths x y mx = nexts (mx!(1,1)) where
--     nexts cMin = do
--         v <- safeGet x y mx
--         if (lx, ly) == (x, y) 
--             then Just [[v]] 
--             else combine v (paths x (y+1) mx) (paths (x+1) y mx)
--     combine v (Just xs) (Just ys) = Just $ deepCons v xs ++ deepCons v ys
--     combine v xs ys = (deepCons v) <$> (xs <|> ys)
--     deepCons x xxs = (x :) <$> xxs

parse :: [String] -> Matrix Int
parse strs = fromLists (((\a -> read [a]::Int) <$>) <$> strs)

shortest = shortest1 . parse

-- test
test1 = shortest ["567", "133", "502"] == 11
test2 = shortest [
    "56713350",
    "23542393",
    "19595213",
    "74241524",
    "52644987",
    "02912978",
    "72796409",
    "27353017"
    ] == 49

test3 = 83 == shortest [
    "5671335023542393",
    "1959521374241524",
    "5264498702912978",
    "7279640927353017",
    "3552203099166939",
    "6303534098932459",
    "4691632053155249",
    "4846314576468426",
    "3381563911964194",
    "2826106396577758",
    "5379557159944708",
    "5754636972892574",
    "8148591449714459",
    "2762056379040276",
    "6257509967887403",
    "8139392819674470"
    ]
test4 = 103 == shortest [
    "56713350235423931959",
    "52137424152452644987",
    "02912978727964092735",
    "30173552203099166939",
    "63035340989324594691",
    "63205315524948463145",
    "76468426338156391196",
    "41942826106396577758",
    "53795571599447085754",
    "63697289257481485914",
    "49714459276205637904",
    "02766257509967887403",
    "81393928196744702720",
    "30047073906843300464",
    "76022066700345668604",
    "89189066188051667940",
    "98936282290349102554",
    "16799240776568385448",
    "70229961621488486883",
    "20194853763510374583"
    ]