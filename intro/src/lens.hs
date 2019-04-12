{-# LANGUAGE TemplateHaskell, RankNTypes #-}

import Control.Lens
import Data.Functor
import Data.Functor.Const

data Point = Point
    { _positionX :: Double
    , _positionY :: Double
    } deriving (Show)
makeLenses ''Point

data Segment = Segment
    { _segmentStart :: Point
    , _segmentEnd :: Point
    } deriving (Show)
makeLenses ''Segment

extremityCoordinates :: Traversal Segment Segment Point Point
extremityCoordinates a2fb s = Segment <$> a2fb (_segmentStart s) <*> a2fb (_segmentEnd s)

sequ :: [ Maybe a ] -> Maybe [a]
sequ [] = Just []
sequ (x:xs) = (:) <$> x <*> (sequ xs)

pX :: Functor f => (Double -> f Double) -> Point -> f Point
pX f p = (\x1 -> p { _positionX = x1 }) <$> f (_positionX p)

myView :: ((a -> Const a a) -> s -> Const a s) -> s -> a
myView l s = getConst $ l Const s

mySet :: ((a -> Identity a) -> s -> Identity s) -> a -> s -> s
mySet l a s = myOver l (const a) s

myOver :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
myOver l f s = runIdentity $ l (Identity . f) s

data Fct a = F1 | F2 a

instance Functor Fct where
    fmap f F1 = F1
    fmap f (F2 a) = F2 (f a)

-- contra
fco :: Applicative f => (a -> f b) -> a -> f b
fco = ($)

