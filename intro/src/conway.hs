import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified System.Random as R
import System.Timeout

conwayM :: Int -> Int -> IO ()
conwayM c r = do 
    ns <- randoms
    print1 $ (fromGame <$> (conway $ genGame c r ns))
    return ()
    where 
        print1 (x:xs) = do
            putStr "\ESC[2J"
            mapM putStrLn x
            c <- timeout 200000 getChar
            maybe (print1 xs) (\c -> if 'q' == c then return () else print1 xs) c
            

seed :: [[Char]]
seed = [
    [ ' ', 'x', ' ' ],
    [ 'x', 'x', 'x' ],
    [ ' ', 'x', ' ' ]
    ]

randoms :: IO [Int]
randoms = R.randoms <$> R.getStdGen 

genGame :: Int -> Int -> [Int] -> Game
genGame r c ns = 
    let xs = take (r * c) $ even <$> ns
        rows = L.unfoldr (\xs1 -> if L.null xs1 then Nothing else Just $ splitAt r xs1) xs
    in toGame rows

toGame :: [[Bool]] -> Game
toGame xs = M.fromList $ L.concat $ byRow <$> zip [0..] ((zip [0..] <$> xs))
    where 
        byRow (y, xs) = cell y <$> xs
        cell y (x, v) = ((x, y), v)

fromGame :: Game -> [[Char]]
fromGame g = L.unfoldr toRow $ toCell <$> rows
    where
        rows = M.toList g
        toRow [] = Nothing
        toRow xs = Just $ L.splitAt rowCnt xs
        rowCnt = (+) 1 $ fromInteger $ L.maximum $ (fst . fst) <$> rows
        toCell ((x, y), v) = if v then 'X' else ' '

type Game = M.Map (Integer, Integer) Bool

conway :: Game -> [Game]
conway seed = seed : conway (next seed)

coords = [ 
    (-1, -1), (-1, 0), (-1, 1),
    (0, -1),           (0, 1),
    (1, -1),  (1, 0),  (1, 1)
    ]

next :: Game -> Game
next g = M.fromList $ call <$> M.toList g
    where
    call ((x, y), live) = 
        let neighbors = cntNeighbors x y g;
                live1 = (live && L.elem neighbors [2, 3]) || (not live && neighbors == 3)
        in ((x, y), live1)
cntNeighbors x y g = L.length . L.filter id $ (getOne x y g) <$> coords
getOne x y g (offX, offY) = maybe False id $ M.lookup (x + offX, y + offY) g