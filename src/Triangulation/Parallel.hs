-- | Divide-and-conquer driver: split the points along the longer side of their
-- bounding box, triangulate the halves (in parallel while they are large),
-- merge.
module Triangulation.Parallel (
  triangulate,
) where

import Control.Parallel.Strategies (rdeepseq, rparWith, runEval)
import Data.List (sortBy)
import Data.List qualified as List
import Triangulation.Geometry.Point (Point (..))
import Triangulation.Leaf (triangulateLeaf)
import Triangulation.Merge (mergeTriangulations)
import Triangulation.Repair (repairDegeneracies)
import Triangulation.Store qualified as Store
import Triangulation.Types (Axis (..), Triangulation (..))

-- | Delaunay triangulation of a point set; 'Nothing' when the points do not
-- span a triangle: fewer than three of them, or all of them on one line.
triangulate :: [Point] -> Maybe Triangulation
triangulate points
  | length points < 3 = Nothing
  | otherwise = do
      triangulation <- repairDegeneracies [] <$> go points
      if Store.null (triangleStore triangulation) then Nothing else Just triangulation

-- | Point sets at least this large have their halves evaluated in parallel.
parallelThreshold :: Int
parallelThreshold = 256

go :: [Point] -> Maybe Triangulation
go points
  | Just leaf <- triangulateLeaf points = Just leaf
  | otherwise = do
      let Extent n minX maxX minY maxY = extentOf points
          axis = if maxX - minX > maxY - minY then X else Y
          (lefts, rights) = split n axis points
          (left, right)
            | n >= parallelThreshold = runEval $ do
                -- one spark for the left half; the right one is evaluated here,
                -- which is what sparking both used to end up doing anyway, only
                -- after paying for a spark that then fizzled
                l <- rparWith rdeepseq (go lefts)
                r <- rdeepseq (go rights)
                pure (l, r)
            | otherwise = (go lefts, go rights)
      l <- left
      r <- right
      mergeTriangulations l r axis

-- | Split into two parts along the axis. Halves, except that 9–11 points go
-- 3 + rest (halving would leave a part of 5, which cannot be split into two
-- leaves), and 5 points go 2 + 3 (see 'triangulateLeaf').
split :: Int -> Axis -> [Point] -> ([Point], [Point])
split n axis points
  | n > selectionThreshold = selectSmallest (pointsOrder axis) leftSize n points
  | otherwise = splitAt leftSize (sortBy (pointsOrder axis) points)
  where
    leftSize = if n `elem` [9, 10, 11] then 3 else n `div` 2

-- | Sets larger than this are partitioned by selecting the median; smaller
-- ones are sorted outright. Sorting a short list is cheap, and it keeps the
-- points reaching a leaf in the order they have always arrived in, which the
-- two-point leaf of a five-point set depends on.
selectionThreshold :: Int
selectionThreshold = 32

-- | The @k@ smallest of @n@ points by the given order, and the rest.
--
-- Neither part comes back ordered, and neither needs to be: the divide step
-- only requires that the two halves be separated by a line, and each half is
-- partitioned again along its own axis. Selecting the median takes a pass per
-- level of the selection instead of the @n log n@ of a full sort, and the
-- sorting used to be about two fifths of the running time.
selectSmallest :: (Point -> Point -> Ordering) -> Int -> Int -> [Point] -> ([Point], [Point])
selectSmallest order = select
  where
    select k n points
      | k <= 0 = ([], points)
      | k >= n = (points, [])
      | otherwise = case drop (n `div` 2) points of
          [] -> (points, [])
          pivot : _ ->
            let (smaller, equal, larger) = partitionAround pivot points
                belowCount = length smaller
                equalCount = length equal
             in case compare k belowCount of
                  LT -> keepLeft (select k belowCount smaller) equal larger
                  EQ -> (smaller, equal ++ larger)
                  GT
                    | k <= belowCount + equalCount ->
                        let (taken, left) = splitAt (k - belowCount) equal
                         in (smaller ++ taken, left ++ larger)
                    | otherwise ->
                        keepRight smaller equal (select (k - belowCount - equalCount) (n - belowCount - equalCount) larger)
    keepLeft (chosen, rest) equal larger = (chosen, rest ++ equal ++ larger)
    keepRight smaller equal (chosen, rest) = (smaller ++ equal ++ chosen, rest)
    partitionAround pivot = foldr step ([], [], [])
      where
        step p (below, same, above) = case order p pivot of
          LT -> (p : below, same, above)
          EQ -> (below, p : same, above)
          GT -> (below, same, p : above)

-- | Along 'X' points go left to right; along 'Y' they go top to bottom.
--
-- Ties are broken the way the symbolic perturbation behind
-- 'Triangulation.Geometry.Point.turn' would break
-- them (a lower rank is displaced further, so among points with the same x
-- the one with the smaller y is further right, and among points with the same
-- y the one with the smaller x is further up). The two halves are then
-- separated by a straight line in the perturbed plane, which the merge relies
-- on; splitting ties arbitrarily would put points of one half between points
-- of the other.
-- Written out rather than through @comparing@ on a tuple: the comparator runs
-- once per comparison of every sort at every level of the recursion, and the
-- tuple and the 'Data.Ord.Down' wrapper were allocated every time.
pointsOrder :: Axis -> Point -> Point -> Ordering
pointsOrder X (Point ax ay) (Point bx by) = case compare ax bx of
  EQ -> compare by ay
  unequal -> unequal
pointsOrder Y (Point ax ay) (Point bx by) = case compare by ay of
  EQ -> compare ax bx
  unequal -> unequal
{-# INLINE pointsOrder #-}

-- | How many points there are and the corners of their bounding box.
data Extent = Extent !Int !Double !Double !Double !Double

-- | One pass for the count and all four extremes; the axis and the split size
-- both come from it. Asking for the length and then for each extreme
-- separately walked the list five times per node of the recursion.
extentOf :: [Point] -> Extent
extentOf = List.foldl' step (Extent 0 inf (-inf) inf (-inf))
  where
    step (Extent n minX maxX minY maxY) (Point x y) =
      Extent (n + 1) (min minX x) (max maxX x) (min minY y) (max maxY y)
    inf = 1 / 0
