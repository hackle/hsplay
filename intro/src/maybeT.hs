import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Data.Char
import Control.Monad.Trans (lift)

maybeIdent :: MaybeT Identity String
maybeIdent = pure "blah"

maybeDivide :: Int -> Int -> MaybeT Identity Int
maybeDivide x y = if y == 0 
                    then MaybeT $ Identity Nothing 
                    else pure $ x `div` y

maybeNotNil :: MaybeT Identity Int
maybeNotNil = do
    x <- return 2
    y <- return $ 2 `div` 1
    res <- maybeDivide x y
    return res

isValid :: String -> Bool
isValid str = any isNumber str &&
                any isPunctuation str &&
                any isLetter str

maybePass :: MaybeT IO String
maybePass = do
    candi <- lift getLine
    guard $ isValid candi
    lift $ putStrLn "The password is valid"
    return candi

keepTrying :: MaybeT IO String
keepTrying = msum $ repeat maybePass