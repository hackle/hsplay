import Data.List

isPP :: Integer -> Maybe (Integer, Integer)
isPP i = find isGood $ (\b -> (b, getPower b)) <$> [2..maxBase] where
    getPower b = round $ logBase (fromInteger b) (fromInteger i)
    maxBase = round $ sqrt (fromInteger i)
    isGood (bs, pw) = bs ^ pw == i