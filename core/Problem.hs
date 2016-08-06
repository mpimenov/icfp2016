module Problem ( Silhouette
               , Skeleton
               , Problem (..)
               , ProblemR (..)
               , ProblemD (..)
               , nextProblem
               )
    where

import Control.Monad
import Data.Ratio
import Geom
import Tokenizer

type Silhouette a = [Polygon a]
type Skeleton a = [Segment a]

data Problem a = Problem (Silhouette a) (Skeleton a)
                 deriving (Show)
type ProblemR = Problem Rational
type ProblemD = Problem Double

nextPoint :: Tokenizer PointR
nextPoint = do
  x <- nextRational
  Comma <- nextToken
  y <- nextRational
  return $ Point x y

nextPolygon :: Tokenizer PolygonR
nextPolygon = do
  n <- nextInt
  replicateM n nextPoint

nextSegment :: Tokenizer SegmentR
nextSegment = liftM2 (,) nextPoint nextPoint

nextProblem :: Tokenizer (Problem Rational)
nextProblem = do
  n <- nextInt
  silhouette <- replicateM n nextPolygon
  m <- nextInt
  skeleton <- replicateM m nextSegment
  return $ Problem silhouette skeleton
