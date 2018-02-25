{-# LANGUAGE PackageImports #-}
import "matrix" Data.Matrix hiding ((<|>))
import qualified Data.Map.Strict as M

type Map = M.Map
type Pos = (Int, Int)

shortest :: Matrix Int -> Int
shortest mx = go mx cell1 where cell1 = M.singleton (1,1) (mx!(1,1))

go :: Matrix Int -> Map Pos Int -> Int
go mx cs = maybe (go mx cs') id attempt where
    cs' = advance mx cs
    attempt = M.lookup (nrows mx, ncols mx) cs'

advance :: Matrix Int -> Map Pos Int-> Map Pos Int
advance mx ps = M.foldlWithKey (update1 mx) M.empty ps

update1 :: Matrix Int -> Map Pos Int -> Pos -> Int -> Map Pos Int
update1 mx mp (r, c) summ = foldl (set1 mx summ) mp [(r+1, c), (r, c+1)]
    
set1 :: Matrix Int -> Int -> Map Pos Int -> Pos -> Map Pos Int
set1 mx summ m pos@(r, c) = maybe m upsert (safeGet r c mx) where
    upsert incr = maybe (ins incr) (upd incr) (M.lookup pos m)
    ins incr = M.insert pos (summ+incr) m
    upd incr s1 = let proj = summ+incr in
        if proj < s1 then M.insert pos proj m else m

parse :: [String] -> Matrix Int
parse strs = fromLists (((\a -> read [a]::Int) <$>) <$> strs)

-- test
sample1 = parse ["567", "133", "502"]
test1 = shortest sample1 == 11

sample2 = parse [
    "56713350",
    "23542393",
    "19595213",
    "74241524",
    "52644987",
    "02912978",
    "72796409",
    "27353017"
    ]

test2 = shortest sample2 == 49

sample3 = parse [
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
    
test3 = 83 == shortest sample3

sample4 = parse [
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

test4 = 103 == shortest sample4