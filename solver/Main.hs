module Main where

import Control.Monad
import Control.Monad.State
import Problem

main :: IO ()
main = do
  problem <- liftM (evalState nextProblem) getContents
  print problem
