module Geom ( Point (..)
            , Polygon
            , Segment
            , cross
            , lengthSquared
            , getArea
            , isCCW
            , sub
            , convexHull
            ) where

import Data.Ratio
import Data.List

data Point = Point { getX :: Rational
                   , getY :: Rational
                   } deriving (Eq, Show, Ord)

sub :: Point -> Point -> Point
sub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

cross :: Point -> Point -> Rational
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

lengthSquared :: Point -> Rational
lengthSquared (Point x y) = x * x + y * y

type Polygon = [Point]

type Segment = (Point, Point)

getArea :: Polygon -> Rational
getArea (origin:points) = sum $ zipWith area points (tail points)
    where area p1 p2 = cross (p1 `sub` origin) (p2 `sub` origin)

isCCW :: Polygon -> Bool
isCCW polygon = (getArea polygon) > 0

ordPoints :: Point -> Point -> Point -> Ordering
ordPoints o p1 p2
  | c > 0 = LT
  | c < 0 = GT
  | c == 0 = compare l2 l1
  where p1' = p1 `sub` o
        p2' = p2 `sub` o 
        c = cross p1' p2'
        l1 = lengthSquared p1' 
        l2 = lengthSquared p2'

convexHullImpl :: Bool -> Point -> Point -> Polygon -> Polygon
convexHullImpl first o mp ps
  | o == mp && not first = []
  | otherwise = o:nps
  where nps = convexHullImpl False (minimumBy (ordPoints o) ps) mp ps  

convexHull :: Polygon -> Polygon
convexHull ps = convexHullImpl True mp mp ps
                where mp = minimum ps
