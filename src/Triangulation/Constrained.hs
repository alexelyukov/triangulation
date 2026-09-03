-- | Constrained triangulation: forcing given edges into a Delaunay
-- triangulation, and triangulating a polygon with holes.
module Triangulation.Constrained (
  constrainedTriangulate,
  forceEdges,
) where

import Data.HashSet qualified as HS
import Data.List qualified as List
import Triangulation.Flip (legalize)
import Triangulation.Geometry.Edge (Edge (..), intersection, mkEdge)
import Triangulation.Geometry.Point (
  Orientation (..),
  Point (..),
  manhattanDistance,
  orientation,
  turn,
 )
import Triangulation.Geometry.Polygon (Polygon, polygonEdges, vertices)
import Triangulation.Geometry.Triangle (
  Triangle,
  isValidCandidate,
  mkTriangle,
  triangleEdges,
  trianglesInside,
  trianglesOutside,
 )
import Triangulation.Parallel (triangulate)
import Triangulation.Repair (repairDegeneracies)
import Triangulation.Store (Store)
import Triangulation.Store qualified as Store
import Triangulation.Types (Triangulation (..))

-- | Triangulate the region inside the boundary polygon and outside the holes.
-- The vertices of the polygons are always part of the triangulation; the
-- given points (which may repeat them) are added. Polygon edges are forced
-- into the triangulation and triangles outside the region are dropped.
constrainedTriangulate :: Polygon -> [Polygon] -> [Point] -> Maybe [Triangle]
constrainedTriangulate boundary holes points = do
  triangulation <- triangulate allPoints
  let store = triangleStore triangulation
      edges = concatMap (subdivide allPoints) (concatMap polygonEdges (boundary : holes))
      missingEdges = filter (not . (`Store.member` store)) edges
      forced = forceEdges store missingEdges edges
      Triangulation _ repaired = repairDegeneracies edges (Triangulation (hull triangulation) forced)
      constrained = Store.triangles repaired
  pure $ List.foldl' (flip trianglesOutside) (trianglesInside boundary constrained) holes
  where
    allPoints = HS.toList . HS.fromList $ points ++ concatMap vertices (boundary : holes)

-- | The chain of edges the polygon edge becomes when the points lying on it
-- (strictly between its endpoints) are made vertices: an edge with a vertex
-- on it cannot exist in a triangulation, so the constraint is the chain.
subdivide :: [Point] -> Edge -> [Edge]
subdivide points (Edge a b) = zipWith mkEdge chain (drop 1 chain)
  where
    chain = a : List.sortOn (manhattanDistance a) (filter (liesBetween a b) points) ++ [b]
    liesBetween u v p =
      p /= u
        && p /= v
        && orientation u v p == Collinear
        && min (px u) (px v) <= px p
        && px p <= max (px u) (px v)
        && min (py u) (py v) <= py p
        && py p <= max (py u) (py v)

-- | Force the given edges into the triangulation: remove every triangle
-- crossed by an edge and re-triangulate the two resulting pockets. The
-- restricted edges are never flipped away afterwards.
forceEdges :: Store -> [Edge] -> [Edge] -> Store
forceEdges store [] _ = store
forceEdges store (edge@(Edge p1 p2) : edges) restrictedEdges =
  let crossedEdges = [(e, p) | e <- Store.edges store, Just p <- [intersection edge e]]
      edgesPoints = concatMap (\(Edge a b, _) -> [a, b]) (List.sortOn snd crossedEdges)
      pointsOn side = dedupeConsecutive $ filter (\p -> turn p1 p2 p == side) edgesPoints
      pockets = [p1 : p2 : reverse (pointsOn Clockwise), p2 : p1 : pointsOn CounterClockwise]
      deletingTriangles = HS.toList $ HS.fromList (concatMap (\(e, _) -> Store.trianglesOn e store) crossedEdges)
      store' = List.foldl' (flip Store.delete) store deletingTriangles
      store'' = List.foldl' (\acc points -> fillPocket acc points restrictedEdges) store' pockets
   in forceEdges store'' edges restrictedEdges

dedupeConsecutive :: Eq a => [a] -> [a]
dedupeConsecutive = concatMap (take 1) . List.group

fillPocket :: Store -> [Point] -> [Edge] -> Store
fillPocket store (p1 : p2 : p3 : ps) restrictedEdges
  | isValidCandidate p1 p2 p3 ps =
      let triangle = mkTriangle p1 p2 p3
          store' = legalize (Store.insert triangle store) (triangleEdges triangle) restrictedEdges
       in fillPocket store' (p1 : p3 : ps) restrictedEdges
  | otherwise = fillPocket store (p2 : p3 : ps ++ [p1]) restrictedEdges
fillPocket store _ _ = store
