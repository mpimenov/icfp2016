module Main where

import Control.Monad
import Control.Monad.State
import Data.Ratio
import Geom
import Tokenizer
import qualified Data.ByteString.Lazy.Char8 as L

type Silhouette = [Polygon]
type Skeleton = [Segment]

data Problem = Problem Silhouette Skeleton
             deriving (Show)

nextPoint :: Tokenizer Point
nextPoint = do
  x <- nextRational
  Comma <- nextToken
  y <- nextRational
  return $ Point x y

nextPolygon :: Tokenizer Polygon
nextPolygon = do
  n <- nextInt
  replicateM n nextPoint

nextSegment :: Tokenizer Segment
nextSegment = liftM2 (,) nextPoint nextPoint

nextProblem :: Tokenizer Problem
nextProblem = do
  n <- nextInt
  silhouette <- replicateM n nextPolygon
  m <- nextInt
  skeleton <- replicateM m nextSegment
  return $ Problem silhouette skeleton

main :: IO ()
main = do
  problem <- liftM (evalState nextProblem) getContents
  print problem
  
