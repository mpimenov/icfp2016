module Solution where

import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Ratio
import Folding
import Geom
import Problem
import qualified Data.Map as M

type Facet = [Int]

data Solution a = Solution [Point a] [Facet] [Point a]
                deriving (Show)

toStrings :: Solution Rational -> [String]
toStrings (Solution source facets destination) =
    [show $ length source] ++
    map pointToString source ++
    [show $ length facets] ++
    (map facetToString facets) ++
    map pointToString destination
    where facetToString facet = unwords $ (show $ length facet) : (map show facet)

          rationalToString r | d == 1 = show n
                             | otherwise = (show n) ++ "/" ++ (show d)
              where n = numerator r
                    d = denominator r

          pointToString (Point x y) = (rationalToString x) ++ "," ++ (rationalToString y)

size :: Solution Rational -> Int
size = sum . map length . toStrings

maxSteps = 100
maxSize = 5000

sortUnique :: (Ord a) => [a] -> [a]
sortUnique = map head . group . sort

sameContents :: (Ord a) => (Polygon a, History a) -> (Polygon a, History a) -> Bool
sameContents (lp, _) (rp, _) = (sortUnique lp) == (sortUnique rp)

unfold, fold :: (Fractional a) => (Polygon a, History a) -> (Polygon a, History a)
unfold (ps, hs) = (foldl (flip mirror) ps hs, hs)
fold (ps, hs) = unfold (ps, reverse hs)

mkSolution :: (Fractional a, Ord a) => [(Polygon a, History a)] -> Solution a
mkSolution phs = Solution source facets destination
    where polygons = nubBy sameContents $ map unfold phs

          source = sortUnique . concat $ map fst polygons
          table = M.fromList $ zip source [0 ..]

          facets = map (map (table M.!) . fst) polygons

          destination = let tags ph = map (table M.!) (fst ph)
                            move ph = zip (tags ph) (fst $ fold ph)
                        in map snd . sortUnique . concat $ map move polygons

isqrt :: Integer -> Maybe Integer
isqrt x | x < 0 = Nothing
       | x == 0 = Just 0
       | otherwise = bsearch 0 (x + 1)
       where bsearch l r | l + 1 == r = Just l
                         | m * m <= x = bsearch m r
                         | otherwise = bsearch l m
                         where m = (l + r) `div` 2

rsqrt :: Rational -> Maybe Rational
rsqrt r = do
  let n = numerator r
      d = denominator r
  sn <- isqrt n
  sd <- isqrt d
  if sn * sn == n && sd * sd == d
  then Just (sn % sd)
  else Nothing

canBeAxis :: SegmentR -> Bool
canBeAxis (s, t) = isJust . rsqrt . lengthSquared $ t `sub` s

-- Returns a valid x axis.
getAxis :: [PolygonR] -> PointR
getAxis silhouette = head $ [t `sub` s | (s, t) <- candidates, fitsPaper (s, t)] ++ [Point 1 0]
    where points = convexHull $ concat silhouette
          candidates = map toUnit . filter canBeAxis $ getEdges points

          toUnit (s, t) = (s, s `add` (d `scale` (1 / l)))
              where d = t `sub` s
                    l = fromJust . rsqrt $ lengthSquared d

          fitsPaper (s, t) = width <= 1 && height <= 1
              where ax = t `sub` s
                    ay = ort ax
                    xs = map (`dot` ax) points
                    ys = map (`dot` ay) points
                    width = maximum xs - minimum xs
                    height = maximum ys - minimum ys

solve :: Problem Rational -> Solution Rational
solve (Problem silhouette _) = Solution source' facets (map (add origin) destination)
    where ax@(Point a c) = getAxis silhouette
          ay@(Point b d) = ort ax

          from' (Point x y) = Point (x * a + y * b) (x * c + y * d)
          to' (Point x y) = Point (x * a + y * c) (x * b + y * d)

          (origin', _) = getBounds $ map (map to') silhouette
          origin = from' origin'

          nsilhouette = map (map (`sub` origin)) silhouette
          paper = [Point 0 0, ax, ax `add` ay, ay]

          solutions = map mkSolution $ wrap (convexHull $ concat nsilhouette) paper
          Solution source facets destination = last $ takeWhile ((<= maxSize) . size) solutions

          source' = map to' source
