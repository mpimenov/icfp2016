module Geom where

import Data.List
import Data.Ratio

data Point a = Point { getX :: a
                     , getY :: a
                     } deriving (Eq, Show, Ord)

type PointR = Point Rational
type PointD = Point Double

data Line a = Line { getOrigin :: Point a
                   , getDirection :: Point a
                   } deriving (Eq, Show)

type LineR = Line Rational
type LineD = Line Double

type Polygon a = [Point a]
type PolygonR = Polygon Rational
type PolygonD = Polygon Double

type Segment a = (Point a, Point a)
type SegmentR = Segment Rational
type SegmentD = Segment Double

segmentToLine :: (Num a) => Segment a -> Line a
segmentToLine (a, b) = Line { getOrigin = a
                            , getDirection = b `sub` a
                            }

sub :: (Num a) => Point a -> Point a -> Point a
sub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

cross, dot :: (Num a) => Point a -> Point a -> a
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

lengthSquared :: (Num a) => Point a -> a
lengthSquared (Point x y) = x * x + y * y

getArea :: (Num a) => Polygon a -> a
getArea (origin:points) = sum $ zipWith area points (tail points)
    where area p1 p2 = cross (p1 `sub` origin) (p2 `sub` origin)

isCCW :: (Num a, Ord a) => Polygon a -> Bool
isCCW polygon = (getArea polygon) > 0

ordPoints :: (Num a, Ord a) => Point a -> Point a -> Point a -> Ordering
ordPoints o p1 p2
  | c > 0 = LT
  | c < 0 = GT
  | c == 0 = compare l2 l1
  where p1' = p1 `sub` o
        p2' = p2 `sub` o 
        c = cross p1' p2'
        l1 = lengthSquared p1' 
        l2 = lengthSquared p2'

convexHull :: (Num a, Ord a) => Polygon a -> Polygon a
convexHull ps = foldl update [] (sortBy (ordPoints o) ps)
    where o = minimum ps
          update [] p = [p]
          update [p1] p2 = [p2, p1]
          update pss@(p2:p1:ps) p3 | cross u v >= 0 = p3:pss
                                   | otherwise = update (p1:ps) p3
              where v = p3 `sub` p2
                    u = p2 `sub` p1

-- Checks whether two polygons are on the same side of the line.
sameSide :: (Num a, Ord a) => Line a -> Polygon a -> Polygon a -> Bool
sameSide (Line o d) a b | all (>= 0) pa && all (>= 0) pb = True
                        | all (<= 0) pa && all (<= 0) pb = True
                        | otherwise = False
    where pa = map (cross d . (`sub` o)) a
          pb = map (cross d . (`sub` o)) b

cut :: (Num a, Fractional a) => Line a -> Polygon a -> [Polygon a]
cut = undefined

mirror :: (Num a, Fractional a) => Line a -> Polygon a -> Polygon a
mirror = undefined

paper = [Point 0 0, Point 1 0, Point 1 1, Point 0 1] :: PolygonR
sample = [Point 0 0, Point 1 0, Point (1 % 2) (1 % 2), Point 0 (1 % 2)] :: PolygonR
