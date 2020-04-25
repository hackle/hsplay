import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified System.Random as R
import System.Timeout

play :: Integer -> Integer -> IO ()
play c r = do 
    ns <- randoms
    print1 $ (toChars c <$> (games $ genGame c r ns))
    return ()
    where 
        print1 (x:xs) = do
            putStr "\ESC[2J" -- clears screen
            mapM putStrLn x
            c <- timeout 200000 getChar
            case c of
                Just 'q' -> return ()
                _ -> print1 xs

type Game = M.Map (Integer, Integer) Bool

games :: Game -> [Game]
games seed = seed : games (next seed)

next :: Game -> Game
next g = M.fromList $ call <$> M.toList g
    where
    call ((x, y), live) = 
        let neighbors = cntNeighbors x y g;
                live1 = (live && L.elem neighbors [2, 3]) || (not live && neighbors == 3)
        in ((x, y), live1)

coords = [ (x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0) ]
cntNeighbors :: Integer -> Integer -> Game -> Int                
cntNeighbors x y g = L.length . L.filter id $ (getOne x y) <$> coords
    where 
        getOne x y (offX, offY) = maybe False id $ M.lookup (x + offX, y + offY) g

randoms :: IO [Bool]
randoms = do 
    ns <- R.randoms <$> R.getStdGen
    return $ even <$> (ns :: [Int])

genGame :: Integer -> Integer -> [Bool] -> Game
genGame r c ns = M.fromList $ zip xys ns
    where 
        xys = [ (x, y) | x <- [ 0 .. r ], y <- [ 0 .. c ]]

toChars :: Integer -> Game -> [[Char]]
toChars r g = L.unfoldr toRow $ toCell <$> M.toList g
    where
        toRow xs = if L.null xs then Nothing else Just $ L.splitAt (fromInteger r) xs
        toCell ((x, y), v) = if v then 'X' else ' '