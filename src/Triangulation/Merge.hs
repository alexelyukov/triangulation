-- | Merging two triangulations of point sets separated by an axis-parallel
-- line: join the hulls with their common tangents and fill the gap between
-- them with triangles.
module Triangulation.Merge (
  mergeTriangulations,
) where

import Data.List.NonEmpty qualified as NE
import Triangulation.Flip (legalize)
import Triangulation.Geometry.Edge (Edge)
import Triangulation.Geometry.Point (
  Point,
  bottomRight,
  cosSquaredAngle,
  leftTop,
  rightBottom,
  topLeft,
 )
import Triangulation.Geometry.Polygon (Polygon (..), tangents)
import Triangulation.Geometry.Ring (arc)
import Triangulation.Geometry.Triangle (Triangle (..), isValidCandidate, mkTriangle, triangleEdges)
import Triangulation.Store (Store)
import Triangulation.Store qualified as Store
import Triangulation.Types (Axis (..), Triangulation (..))

-- | Merge two triangulations whose point sets were split along the given axis:
-- the first one holds the points with smaller x (for 'X') or larger y (for 'Y').
-- 'Nothing' only if the hulls are inconsistent (a bridge endpoint is not a hull vertex).
mergeTriangulations :: Triangulation -> Triangulation -> Axis -> Maybe Triangulation
mergeTriangulations (Triangulation hull1@(Polygon points1) store1) (Triangulation hull2@(Polygon points2) store2) axis = do
  let bridge = case axis of
        X -> (rightBottom points1, leftTop points2)
        Y -> (bottomRight points1, topLeft points2)
  ((bl, br), (tl, tr), mergedHull) <- tangents hull1 hull2 bridge
  remains1 <- arc bl tl points1
  remains2 <- arc tr br points2
  let unitedStore = Store.union store1 store2
  pure $
    Triangulation
      mergedHull
      (fillGap unitedStore (NE.toList remains1) (reverse (NE.toList remains2)) [])

-- | Fill the gap between two hull chains (left and right, sharing no points)
-- with triangles, advancing along whichever chain gives the better triangle.
fillGap :: Store -> [Point] -> [Point] -> [Edge] -> Store
fillGap store lefts rights restrictedEdges = case (lefts, rights) of
  ([], _) -> store
  (_, []) -> store
  ([_], [_]) -> store
  (left : lefts'@(nextLeft : _), [right]) ->
    addCandidate store (left, right, nextLeft) lefts' [right] restrictedEdges
  ([left], right : rights'@(nextRight : _)) ->
    addCandidate store (left, right, nextRight) [left] rights' restrictedEdges
  (left : lefts'@(nextLeft : _), right : rights'@(nextRight : _)) ->
    let leftIsValid = isValidCandidate left right nextLeft rights'
        rightIsValid = isValidCandidate left right nextRight lefts'
        (leftCandidate, rightCandidate) = (mkTriangle left right nextLeft, mkTriangle left right nextRight)
        advanceLeft = (leftCandidate, lefts', rights)
        advanceRight = (rightCandidate, lefts, rights')
        (triangle, lefts'', rights'')
          | leftIsValid && not rightIsValid = advanceLeft
          | rightIsValid && not leftIsValid = advanceRight
          | minCosSquared leftCandidate >= minCosSquared rightCandidate = advanceLeft
          | otherwise = advanceRight
        store' = legalize (Store.insert triangle store) (triangleEdges triangle) restrictedEdges
     in fillGap store' lefts'' rights'' restrictedEdges

-- | Add the candidate triangle if it is valid, then continue filling the gap.
addCandidate :: Store -> (Point, Point, Point) -> [Point] -> [Point] -> [Edge] -> Store
addCandidate store (p1, p2, p3) lefts rights restrictedEdges
  | isValidCandidate p1 p2 p3 [] =
      let triangle = mkTriangle p1 p2 p3
          store' = legalize (Store.insert triangle store) (triangleEdges triangle) restrictedEdges
       in fillGap store' lefts rights restrictedEdges
  | otherwise = fillGap store lefts rights restrictedEdges

-- | The smallest squared cosine over the three angles; larger means the
-- triangle is closer to right-angled, which the merge prefers.
minCosSquared :: Triangle -> Double
minCosSquared (Triangle p1 p2 p3) =
  minimum [cosSquaredAngle p1 p2 p3, cosSquaredAngle p2 p1 p3, cosSquaredAngle p3 p1 p2]
