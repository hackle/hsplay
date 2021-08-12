sequ :: [ Maybe a ] -> Maybe [a]
sequ [] = Just []
sequ (x:xs) = (:) <$> x <*> sequ xs