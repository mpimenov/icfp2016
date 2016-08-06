module Solution where

import Geom

type Facet = [Int]

data Solution a = Solution [Point a] [Facet] [Point a]
                deriving (Show)
