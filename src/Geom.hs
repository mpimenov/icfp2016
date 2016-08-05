module Geom where

import Data.Ratio

data Point = Point { getX :: Rational
                   , getY :: Rational
                   } deriving (Eq, Show)

type Polygon = [Point]

type Segment = (Point, Point)
