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

data Point = Point { getX :: Rational
                   , getY :: Rational
                   } deriving (Eq, Show)

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


chooseNext :: Point -> [Point] -> Point
chooseNext o [] = o
chooseNext o (p1:ps)
  | p2 == o = p1
  | cr > 0 || (cr == 0 && len1 > len2) = p1
  | otherwise = p2
  where p2 = chooseNext o ps
        p1' = sub p1 o
        p2' = sub p2 o
        cr = cross p1' p2'
        len1 = lengthSquared p1'
        len2 = lengthSquared p2'
        
minPoint :: [Point] -> Point
minPoint [] = (Point 100 100)
minPoint (p:ps)
  | (getX p) < (getX q) || ((getX p) == (getX q) && (getY p) < (getY q)) = p
  | otherwise = q
  where q = minPoint ps
  
  
convexHullImpl :: Point -> Point -> Point -> Polygon -> Polygon
convexHullImpl prev o mp ps
  | o == mp && prev /= (Point 100 100) = []
  | otherwise = o:nps
  where nps = convexHullImpl o (chooseNext o ps) mp ps

convexHull :: Polygon -> Polygon
convexHull ps = convexHullImpl (Point 100 100) mp mp ps
                where mp = minPoint ps
