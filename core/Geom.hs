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

add, sub :: (Num a) => Point a -> Point a -> Point a
add (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
sub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

cross, dot :: (Num a) => Point a -> Point a -> a
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

lengthSquared :: (Num a) => Point a -> a
lengthSquared (Point x y) = x * x + y * y

getArea :: (Num a) => Polygon a -> a
getArea [] = 0
getArea (origin:points) = sum $ zipWith area points (tail points)
    where area p1 p2 = cross (p1 `sub` origin) (p2 `sub` origin)

getBounds :: (Ord a) => [Polygon a] -> (Point a, Point a)
getBounds polygons = (Point (minimum xs) (minimum ys), Point (maximum xs) (maximum ys))
    where points = concat polygons
          xs = map getX points
          ys = map getY points

isCCW :: (Num a, Ord a) => Polygon a -> Bool
isCCW polygon = (getArea polygon) > 0

ordPoints :: (Num a, Ord a) => Point a -> Point a -> Point a -> Ordering
ordPoints o p1 p2
    | c > 0 = LT
    | c < 0 = GT
    | c == 0 = compare l1 l2
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

scale :: (Num a) => Point a -> a -> Point a
scale (Point x y) f = Point (f * x) (f * y)

mirrorPoint :: (Fractional a) => Line a -> Point a -> Point a
mirrorPoint (Line o d) p = ((scale proj 2) `sub` p') `add` o
    where p' = p `sub` o
          proj = scale d ((dot p' d) / (dot d d))

mirror :: (Fractional a) => Line a -> Polygon a -> Polygon a
mirror l p = map (mirrorPoint l) p

lineToABC :: (Fractional a) => Line a -> (a, a, a)
lineToABC (Line o d) = (a, b, c)
    where (Point x1 y1) = o
          (Point x2 y2) = o `add` d
          a = y1 - y2
          b = x2 - x1
          c = - a * x1 - b * y1

data IntersectionResult a = Coincide
                          | Parallel
                          | Intersect (Point a)
                            deriving (Show, Eq)

equalLines :: (Fractional a, Eq a) => Line a -> Line a -> Bool
equalLines (Line o1 d1) (Line o2 d2) = (cross d1 d2) == 0 && (cross o1o2 d1) == 0
    where o1o2 = o2 `sub` o1

intersectLines :: (Fractional a, Eq a) => Line a -> Line a -> IntersectionResult a
intersectLines l1 l2 | equalLines l1 l2 = Coincide
                     | d == 0           = Parallel
                     | otherwise        = Intersect (Point (dx / d) (-dy / d))
        where (a1, b1, c1) = lineToABC l1
              (a2, b2, c2) = lineToABC l2
              d  = a1 * b2 - a2 * b1
              dx = b1 * c2 - b2 * c1
              dy = a1 * c2 - a2 * c1

sidePointLine :: (Fractional a, Ord a) => Point a -> Line a -> Integer
sidePointLine p (Line o d) | cr == 0   = 0
                           | cr > 0    = 1
                           | otherwise = -1
    where cr = (p `sub` o) `cross` d

getEdges :: (Num a) => Polygon a -> [(Point a, Point a)]
getEdges [] = []
getEdges pss@(p:ps) = zip pss (ps ++ [p])

cutImpl :: (Fractional a, Ord a, Show a) => Line a -> [(Point a, Point a)] -> (Polygon a, Polygon a)
cutImpl l [] = ([], [])
cutImpl l ((p1, p2):es)
    | s1 == 0             = (p1:pol1, p1:pol2)
    | s1 > 0 && s2 >= 0   = (p1:pol1, pol2)
    | s1 < 0 && s2 <= 0   = (pol1, p1:pol2)
    | s1 > 0 && s2 < 0    = (p1:int:pol1, int:pol2)
    | s1 < 0 && s2 > 0    = (int:pol1, p1:int:pol2)
    | otherwise           = error $ "unhandled case in intersection " ++ (show p1) ++ " " ++ (show p2) ++ " " ++ (show l)
    where s1 = sidePointLine p1 l
          s2 = sidePointLine p2 l
          (Intersect int) = intersectLines l (segmentToLine (p1, p2))
          (pol1, pol2) = cutImpl l es

cut :: (Fractional a, Ord a, Show a) => Line a -> Polygon a -> [Polygon a]
cut l p = filter ((/= 0) . getArea) [pol1, pol2]
    where (pol1, pol2) = cutImpl l (getEdges p)

testP1 = (Point 0 0) :: PointD
testP2 = (Point 1 0)
testP3 = (Point 1 1)
testP4 = (Point 0 1)
testL1 = (Line testP1 (sub testP2 testP1))
testL2 = (Line testP2 (sub testP3 testP2))
testL3 = (Line testP3 (sub testP4 testP3))
testL4 = (Line testP4 (sub testP1 testP4))
testL5 = (Line testP1 (sub testP3 testP1))
testL6 = (Line testP2 (sub testP4 testP2))
testPoly1 = [testP1, testP2, testP3, testP4]
paper = [Point 0 0, Point 1 0, Point 1 1, Point 0 1] :: PolygonR
sample = [Point 0 0, Point 1 0, Point (1 % 2) (1 % 2), Point 0 (1 % 2)] :: PolygonR
