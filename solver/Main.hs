module Main where

import Control.Monad
import Control.Monad.State
import Problem
import Solution

main :: IO ()
main = do
  problem <- liftM (evalState nextProblem) getContents

  let solution = solve problem

  mapM_ putStrLn $ toStrings solution
