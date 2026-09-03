-- | Delaunay edge flips: restoring the empty-circumcircle property locally by
-- swapping the diagonal of a convex quadrilateral.
module Triangulation.Flip (
  trianglesOnQuadrilateral,
  legalize,
) where

import Data.List qualified as List
import Triangulation.Geometry.Edge (Edge (..))
import Triangulation.Geometry.Point (Orientation (..), Point, orientation)
import Triangulation.Geometry.Polygon (Hull4 (..), hullOf4)
import Triangulation.Geometry.Triangle (
  Triangle (..),
  isOutsideCircumcircle,
  mkTriangle,
  triangleEdges,
 )
import Triangulation.Store (Store)
import Triangulation.Store qualified as Store

-- | The two triangles of a convex quadrilateral (vertices in boundary order),
-- split along the diagonal that satisfies the Delaunay condition.
--
-- When three of the four vertices are collinear only one diagonal gives two
-- triangles of non-zero area, and that one is chosen regardless of the
-- in-circle test.
trianglesOnQuadrilateral :: Point -> Point -> Point -> Point -> (Triangle, Triangle)
trianglesOnQuadrilateral p1 p2 p3 p4
  | hasFlat acrossP2P4 = acrossP1P3
  | hasFlat acrossP1P3 = acrossP2P4
  | isOutsideCircumcircle p1 (mkTriangle p2 p3 p4) = acrossP2P4
  | otherwise = acrossP1P3
  where
    acrossP2P4 = (mkTriangle p1 p2 p4, mkTriangle p2 p3 p4)
    acrossP1P3 = (mkTriangle p1 p2 p3, mkTriangle p1 p3 p4)
    hasFlat (t1, t2) = isFlat t1 || isFlat t2

-- | Restore the Delaunay condition around the given edges by flipping the
-- diagonal of every non-Delaunay pair of adjacent triangles, propagating to
-- the edges of the new triangles. Restricted edges are never flipped.
legalize :: Store -> [Edge] -> [Edge] -> Store
legalize store [] _ = store
legalize store (edge : es) restrictedEdges
  | edge `elem` restrictedEdges = legalize store es restrictedEdges
  | otherwise = case Store.trianglesOn edge store of
      [tr1, tr2]
        | Edge a b <- edge
        , c <- apex edge tr1
        , d <- apex edge tr2
        , Quadrilateral p1 p2 p3 p4 <- hullOf4 a b c d
        , (new1, new2) <- trianglesOnQuadrilateral p1 p2 p3 p4
        , new1 `notElem` [tr1, tr2]
        , not (isFlat new1 || isFlat new2) ->
            let newEdges = dedupe (triangleEdges new1 ++ triangleEdges new2) ++ es
                store' = List.foldl' (flip Store.delete) store [tr1, tr2]
                store'' = List.foldl' (flip Store.insert) store' [new1, new2]
             in legalize store'' newEdges restrictedEdges
      _ -> legalize store es restrictedEdges

-- | The vertex of the triangle that is not an endpoint of the edge. The two
-- endpoints and the two apexes are the four distinct points of the pair of
-- triangles sharing the edge, which is what the flip needs; building a set to
-- find them, as this used to, costs an allocation per flip.
apex :: Edge -> Triangle -> Point
apex (Edge u v) (Triangle p1 p2 p3)
  | p1 /= u && p1 /= v = p1
  | p2 /= u && p2 /= v = p2
  | otherwise = p3

-- | The distinct elements of a list of six edges at most; cheaper than a set
-- for that size, and unlike the previous code it does not walk the whole
-- pending queue on every flip.
dedupe :: [Edge] -> [Edge]
dedupe = foldr (\e rest -> e : filter (/= e) rest) []

isFlat :: Triangle -> Bool
isFlat (Triangle a b c) = orientation a b c == Collinear
