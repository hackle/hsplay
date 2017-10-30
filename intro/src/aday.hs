import Data.Bifunctor
import Data.Functor.Identity

rseq :: (Num a) => [[(a, b)]] -> [(a, [b])]
-- [(a, b)] -> (a, [b])
-- list (a, ?) -> (a, list ?)
rseq = map sequenceA' where
    -- [(a,b)] -> [(Sum a, b)] -> (Sum a,[b]) -> (a, [b])
    sequenceA' = (first runIdentity) . sequenceA . (map (first Identity))