-- Some of the examples in this chapter require a few GHC extensions:
-- TemplateHaskell is needed for makeLenses; RankNTypes is needed for
-- a few type signatures later on. 
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

import Control.Lens

data Point = Point
    { _positionX :: Double
    , _positionY :: Double
    } deriving (Show)
makeLenses ''Point

data Segment = Segment
    { _segmentStart :: Point
    , _segmentEnd :: Point
    } deriving (Show)

pointCoordinates :: Applicative f => (Double -> f Double) -> Point -> f Point
pointCoordinates f (Point x y) = Point <$> f x <*> f y

extremityCoordinates :: Applicative f => (Double -> f Double) -> Segment -> f Segment
extremityCoordinates f (Segment s1 s2) = Segment <$> pointCoordinates f s1 <*> pointCoordinates f s2

makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)

-- makeLenses ''Segment

-- positionX :: Lens Double Double Point Point
-- positionX :: Functor f => (Double -> f Double) -> Point -> f Point
-- positionX f p@(Point x _) = (\x' -> p { _positionX = x' }) <$> (f x) 

-- lens1 :: Functor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
-- lens1 f g h s = (g s) <$> (h $ f s)

-- positionX' :: Functor f => (Double -> f Double) -> Point -> f Point
-- positionX' = lens1 _positionX (\p1 x1 -> p1 { _positionX = x1 })