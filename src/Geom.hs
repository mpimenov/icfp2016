module Geom ( Point (..)
            , Polygon
            , Segment
            , cross
            , getArea
            , isCCW
            , sub
            ) where

import Data.Ratio

data Point = Point { getX :: Rational
                   , getY :: Rational
                   } deriving (Eq, Show)

sub :: Point -> Point -> Point
sub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

cross :: Point -> Point -> Rational
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

type Polygon = [Point]

type Segment = (Point, Point)

getArea :: Polygon -> Rational
getArea (origin:points) = sum $ zipWith area points (tail points)
    where area p1 p2 = cross (p1 `sub` origin) (p2 `sub` origin)

isCCW :: Polygon -> Bool
isCCW polygon = (getArea polygon) > 0
