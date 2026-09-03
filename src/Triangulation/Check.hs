-- | Validity checks on sets of triangles. Quadratic; meant for tests and debugging.
module Triangulation.Check (
  isDelaunay,
  isLocallyDelaunay,
  hasNoIntersections,
) where

import Data.HashSet qualified as HS
import Data.Maybe (isNothing)
import Triangulation.Geometry.Edge (Edge (..), intersection)
import Triangulation.Geometry.Triangle (Triangle (..), isOutsideCircumcircle, triangleEdges)
import Triangulation.Store qualified as Store

-- | No vertex lies strictly inside the circumcircle of any triangle: the
-- (unconstrained) Delaunay property.
isDelaunay :: [Triangle] -> Bool
isDelaunay ts =
  let points = HS.toList . HS.fromList $ concatMap (\(Triangle p1 p2 p3) -> [p1, p2, p3]) ts
      others (Triangle p1 p2 p3) = filter (`notElem` [p1, p2, p3]) points
   in all (\t -> all (`isOutsideCircumcircle` t) (others t)) ts

-- | Every edge shared by two triangles is locally Delaunay: the apex of each
-- triangle lies outside (or on) the circumcircle of the other. Together with
-- the constraint that segments are never flipped this characterises a
-- constrained Delaunay triangulation.
isLocallyDelaunay :: [Triangle] -> Bool
isLocallyDelaunay ts = all locallyDelaunay (Store.edges store)
  where
    store = foldr Store.insert Store.empty ts
    locallyDelaunay e = case Store.trianglesOn e store of
      [t1, t2] -> isOutsideCircumcircle (apex e t2) t1 && isOutsideCircumcircle (apex e t1) t2
      _ -> True
    apex (Edge a b) (Triangle x y z) = case filter (`notElem` [a, b]) [x, y, z] of
      p : _ -> p
      [] -> x

-- | No two edges cross.
hasNoIntersections :: [Triangle] -> Bool
hasNoIntersections = go
  where
    go [] = True
    go (t : ts) = all (notCrossing t) ts && go ts
    notCrossing t1 t2 = and [isNothing (intersection e1 e2) | e1 <- triangleEdges t1, e2 <- triangleEdges t2]
