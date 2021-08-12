-- {-# Language TypeFamilies #-}
-- {-# Language FlexibleInstances #-}

-- module FixIt where

-- type family ElementOf a where
--     ElementOf [[a]] = ElementOf [a]
--     ElementOf [a] = a

-- class Flatten a where
--     flatten :: a -> [ElementOf a]

-- instance (ElementOf [a] ~ a) => Flatten [a] where
--     flatten x = x

-- instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
--     flatten = flatten . concat