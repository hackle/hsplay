import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified System.Random as R
import System.Timeout
import Data.Char
import Data.Function

type Game = M.Map (Int, Int) Bool
type Cell = ((Int, Int), Bool)

play :: Int -> Int -> IO ()
play cols rows = do 
    vals <- randoms
    genGame cols rows vals & 
        games & 
        printGame
    where 
        printGame (g:gs) = do
            putStr "\ESC[2J" -- clears screen
            mapM putStr $ showGame cols g showCell
            c <- timeout 50000000 getChar
            case c of
                Just 'q' -> return ()
                _ -> printGame gs

games :: Game -> [Game]
games = L.iterate next

next :: Game -> Game
next g = nextCell <$> M.toList g & M.fromList
    where
    nextCell ((x, y), live) = 
        let neighbors = cntNeighbors x y g;
                live1 = neighbors == 3 || (live && neighbors == 2)
        in ((x, y), live1)

neighborOffsets :: [(Int, Int)]
neighborOffsets = [ (x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0) ]

cntNeighbors :: Int -> Int -> Game -> Int                
cntNeighbors x y g = getOne <$> neighborOffsets & L.filter id & L.length  
    where 
        getOne (offX, offY) = maybe False id $ M.lookup (x + offX, y + offY) g

randoms :: IO [Bool]
randoms = do 
    vals <- R.randoms <$> R.getStdGen
    return $ even <$> (vals :: [Int])

genGame :: Int -> Int -> [Bool] -> Game
genGame cols rows vals = zip cells vals & M.fromList
    where 
        cells = [ (y, x) | y <- [ 0 .. rows-1 ], x <- [ 0 .. cols-1 ]]

linesWithBreak :: Int -> [String]
linesWithBreak cols = (L.replicate (cols - 1) "") ++ [ "\n" ] & L.cycle

showGame :: Int -> Game -> (Game -> Cell -> String) -> [String]
showGame cols g showCell = L.zipWith (++) cells (linesWithBreak cols)
    where cells = showCell g <$> M.toList g
        
showCell :: Game -> Cell -> String
showCell _ (_, v) = if v then " " else "█"

showDebug :: Game -> Cell -> String
showDebug g ((x, y), v) = show (x, y, v, cntNeighbors x y g)
