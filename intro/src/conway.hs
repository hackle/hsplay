import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified System.Random as R
import System.Timeout
import Data.Char
import Data.Function

play :: Int -> Int -> IO ()
play cols rows = do 
    ns <- randoms
    print1 $ (games $ genGame cols rows ns)
    return ()
    where 
        print1 (g:gs) = do
            -- putStr "\ESC[2J" -- clears screen
            -- mapM putStrLn (L.concat $ showDebug cols x)
            mapM putStr $ showGame cols g showValue
            c <- timeout 50000000 getChar
            case c of
                Just 'q' -> return ()
                _ -> print1 gs

type Game = M.Map (Int, Int) Bool
type Cell = ((Int, Int), Bool)

games :: Game -> [Game]
games = L.iterate next

next :: Game -> Game
next g = M.fromList $ call <$> M.toList g
    where
    call ((x, y), live) = 
        let neighbors = cntNeighbors x y g;
                live1 = neighbors == 3 || (live && neighbors == 2)
        in ((x, y), live1)

coords = [ (x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0) ]

cntNeighbors :: Int -> Int -> Game -> Int                
cntNeighbors x y g = L.length . L.filter id $ (getOne x y) <$> coords
    where 
        getOne x y (offX, offY) = maybe False id $ M.lookup (x + offX, y + offY) g

randoms :: IO [Bool]
randoms = do 
    ns <- R.randoms <$> R.getStdGen
    return $ even <$> (ns :: [Int])

genGame :: Int -> Int -> [Bool] -> Game
genGame cols rows ns = M.fromList $ zip xys ns
    where 
        xys = [ (y, x) | y <- [ 0 .. rows-1 ], x <- [ 0 .. cols-1 ]]

linesWithBreak :: Int -> [String]
linesWithBreak cols = (L.replicate (cols - 1) "") ++ [ "\n" ] & L.cycle

showGame :: Int -> Game -> (Game -> Cell -> String) -> [String]
showGame cols g showCell = showCell g <$> M.toList g &
                flip (L.zipWith (++)) (linesWithBreak cols)
        
showValue :: Game -> Cell -> String
showValue _ (_, v) = if v then " " else "█"

showDebug :: Game -> Cell -> String
showDebug g ((x, y), v) = show (x, y, v, cntNeighbors x y g)
