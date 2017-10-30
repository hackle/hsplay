afoldr :: (a -> b -> b) -> b -> [a] -> b
afoldr _ b [] = b
afoldr f b (x:xs) = f x (afoldr f b xs)

afoldl :: (b -> a -> b) -> b -> [a] -> b
afoldl _ b [] = b
afoldl f b (x:xs) = afoldl f (f b x) xs

bfoldl :: (b -> a -> b) -> b -> [a] -> b
bfoldl _ b [] = b
bfoldl f b xs = foldr (\a fb -> \b1 -> fb (f b1 a)) id xs b

-- bfoldl (\b a -> (show a) ++ "(" ++ b  ++ ")") "" [1..5]
-- (\1 id -> \b1 -> id (f b1 1))
-- (\2 fb1) -> \b2 -> fb1 (f b2 2))
-- (\3 fb2) -> \b3 -> fb2 (f b3 3))