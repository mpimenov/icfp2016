module Main where

import Control.Monad
import Control.Monad.State
import Data.Ratio
import Folding
import Geom
import Problem
import Solution
import Text.Printf

rationalToString :: Rational -> String
rationalToString r | d == 1 = show n
                   | otherwise = (show n) ++ "/" ++ (show d)
    where n = numerator r
          d = denominator r

pointToString :: Point Rational -> String
pointToString (Point x y) = (rationalToString x) ++ "," ++ (rationalToString y)

printPoint :: Point Rational -> IO ()
printPoint = putStrLn . pointToString

main :: IO ()
main = do
  (Problem silhouette _) <- liftM (evalState nextProblem) getContents
  let (Solution source facets destination) =
          mkSolution $ wrap (convexHull $ concat silhouette) paper
  print $ length source
  mapM_ printPoint source

  print $ length facets
  forM_ facets $ \facet -> putStrLn . unwords $ map show (length facet:facet)

  mapM_ printPoint destination
