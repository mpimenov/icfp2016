module Folding where

import Data.List
import Geom

-- Histroy of mirrors
type History a = [Line a]

maxSteps = 100

-- A single wrap step - selects a single polygon that can be folded to
-- cover some part of |figure|, and performs that folding.
step :: (Fractional a, Ord a, Show a) => Polygon a -> [(Polygon a, History a)] -> [(Polygon a, History a)]
step figure polygons = case [(p, h, l) | l <- lines, (p, h) <- candidates l] of
                         []    -> polygons
                         ((p, h, l):_) -> map (update h l) (cut l p) ++ (delete (p, h) polygons)
    where lines = map segmentToLine $ getEdges figure
          candidates line = filter (not . sameSide line figure . fst) polygons
          update h l p | sameSide l figure p = (p, h)
                       | otherwise = (mirror l p, l : h)

-- A wrapping algorithm - takes a convex |figure| and a convex |paper|
-- and tries to fold paper to cover the |figure|. Does at most
-- |maxSteps| steps, returns a list of final facets (possibly
-- intersecting) with a histry, that can be used to recover a crease
-- pattern.
wrap :: (Fractional a, Ord a, Show a) => Polygon a -> Polygon a -> [(Polygon a, History a)]
wrap figure paper = last . take maxSteps $ iterate (step figure) [(paper, [])]
