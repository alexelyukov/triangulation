-- | Triangulations of the smallest point sets, the leaves of the
-- divide-and-conquer tree.
module Triangulation.Leaf (
  triangulateLeaf,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Triangulation.Flip (trianglesOnQuadrilateral)
import Triangulation.Geometry.Point (Point)
import Triangulation.Geometry.Polygon (Hull4 (..), Polygon (..), hull4Polygon, hullOf3, hullOf4)
import Triangulation.Geometry.Triangle (Triangle, mkTriangle)
import Triangulation.Types (Triangulation, fromTriangles)

-- | Triangulation of 3 or 4 points; 'Nothing' for any other number.
--
-- Two points give a degenerate triangulation: a two-vertex hull and no
-- triangles. It only arises when a five-point set is split 2 + 3, and the
-- merge fills the gap between the segment and the triangle.
triangulateLeaf :: [Point] -> Maybe Triangulation
triangulateLeaf [a, b] = Just $ fromTriangles (Polygon (a :| [b])) []
triangulateLeaf [a, b, c] = Just $ fromTriangles (hullOf3 a b c) [mkTriangle a b c]
triangulateLeaf [a, b, c, d] =
  let hull4 = hullOf4 a b c d
   in Just $ fromTriangles (hull4Polygon hull4) (trianglesOfHull4 hull4)
triangulateLeaf _ = Nothing

trianglesOfHull4 :: Hull4 -> [Triangle]
trianglesOfHull4 (Quadrilateral p1 p2 p3 p4) =
  let (t1, t2) = trianglesOnQuadrilateral p1 p2 p3 p4 in [t1, t2]
trianglesOfHull4 (TriangleWithInner p1 p2 p3 inner) =
  [mkTriangle p1 p2 inner, mkTriangle p2 p3 inner, mkTriangle p3 p1 inner]
