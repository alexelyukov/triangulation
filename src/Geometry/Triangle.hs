module Geometry.Triangle (
  getEdges,
  buildTrianglesOn3Points,
  buildTrianglesOn4Points,
  mergeTriangles,
  rebuildTriangles,
  isTriangleInPolygon,
  checkNotIntersectedConditions,
  checkDelaunayConditions
) where

import Data.List (nub, sortBy)
import Data.Maybe ( fromMaybe, isJust, isNothing )
import Geometry.Types.Point ( Point(..) )
import Geometry.Types.Polygon ( Polygon(..) )
import Geometry.Types.Edge ( Edge(..) )
import Geometry.Types.PointPosition ( PointPosition(..) )
import Geometry.Types.Triangle ( Triangle(..) )
import Geometry.Polygon ( buildConvexHull, isPointInPolygon )
import Geometry.Edge ( calcPointPosition, normalizeEdge, calcCos2, getIntersectionPoint )
import Geometry.Point ( isPointNotInCircle )
import Utils ( removeSeqDouble )
import TriangulationStore (TriangulationStore, insertTriangle, deleteTriangle, getTriangles, getEdgesFromStore )

-- getters
getPoints :: [Triangle] -> [Point]
getPoints triangles = nub $ concatMap (\(Triangle (p1, p2, p3)) -> [p1, p2, p3]) triangles

getEdges :: Triangle -> [Edge]
getEdges (Triangle (p1, p2, p3)) = [Edge (p1, p2), Edge (p2, p3), Edge (p3, p1)]

getMinAngle :: Triangle -> Double
getMinAngle (Triangle (p1, p2, p3)) =
  let cos2A = calcCos2 (Edge (p1, p2)) (Edge (p1, p3))
      cos2B = calcCos2 (Edge (p2, p1)) (Edge (p2, p3))
      cos2C = calcCos2 (Edge (p3, p1)) (Edge (p3, p2))
  in minimum [cos2A, cos2B, cos2C]

-- simple triangles
buildTrianglesOn3Points :: [Point] -> [Triangle]
buildTrianglesOn3Points [p1, p2, p3] = [Triangle (p1, p2, p3)]
buildTrianglesOn3Points _ = error "Triangulation error"

buildTrianglesOn4Points :: [Point] -> [Point] -> [Triangle]
buildTrianglesOn4Points [p1, p2, p3] initialPoints =
  let innerPoint = head $ filter (`notElem` [p1, p2, p3]) initialPoints
  in [Triangle (p1, p2, innerPoint), Triangle (p2, p3, innerPoint), Triangle (p3, p1, innerPoint)]
buildTrianglesOn4Points [p1, p2, p3, p4] _ =
  if isPointNotInCircle p1 (Triangle (p2, p3, p4))
  then [Triangle (p1, p2, p4), Triangle (p2, p3, p4)]
  else [Triangle (p1, p2, p3), Triangle (p1, p3, p4)]
buildTrianglesOn4Points _ _ = error "Triangulation error"

-- merging process
mergeTriangles :: TriangulationStore -> [Point] -> [Point] -> [Edge] -> TriangulationStore
mergeTriangles store [] _ _ = store
mergeTriangles store _ [] _ = store
mergeTriangles store [_] [_] _ = store
mergeTriangles store (pl : pls) [pr] restrictedEdges =
  let triangle = Triangle (pl, pr, head pls)
  in mergeTriangle store triangle pls [pr] [] restrictedEdges
mergeTriangles store [pl] (pr : prs) restrictedEdges =
  let triangle = Triangle (pl, pr, head prs)
  in mergeTriangle store triangle [pl] prs [] restrictedEdges
mergeTriangles store (pl : pls) (pr : prs) restrictedEdges =
  let (triangleLeft, triangleRight) = (Triangle (pl, pr, head pls), Triangle (pl, pr, head prs))
      (isTriangleLeftCorrect, isTriangleRightCorrect) = (checkTriangleCorrectness triangleLeft prs, checkTriangleCorrectness triangleRight pls)
      (minAngleLeft, minAngleRight) = (getMinAngle triangleLeft, getMinAngle triangleRight)
      predicat = (minAngleLeft >= minAngleRight, isTriangleLeftCorrect, isTriangleRightCorrect)
      (triangle, pls', prs') = selectTriangle predicat (triangleLeft, pls, pr : prs) (triangleRight, pl : pls, prs)
      store' = rebuildSiblings (insertTriangle triangle store) (getEdges triangle) restrictedEdges
  in mergeTriangles store' pls' prs' restrictedEdges

selectTriangle :: (Bool, Bool, Bool) -> (Triangle, [Point], [Point]) -> (Triangle, [Point], [Point]) -> (Triangle, [Point], [Point])
selectTriangle (_, True, False) (triangleLeft, pls, prs) _ = (triangleLeft, pls, prs)
selectTriangle (_, False, True) _ (triangleRight, pls, prs) = (triangleRight, pls, prs)
selectTriangle (True, _, _) (triangleLeft, pls, prs) _ = (triangleLeft, pls, prs)
selectTriangle (_, _, _) _ (triangleRight, pls, prs) = (triangleRight, pls, prs)

mergeTriangle :: TriangulationStore -> Triangle -> [Point] -> [Point] -> [Point] -> [Edge] -> TriangulationStore
mergeTriangle store triangle pls prs nextPoints restrictedEdges
  | checkTriangleCorrectness triangle nextPoints =
    let store' = rebuildSiblings (insertTriangle triangle store) (getEdges triangle) restrictedEdges
    in mergeTriangles store' pls prs restrictedEdges
  | otherwise = mergeTriangles store pls prs restrictedEdges

rebuildSiblings :: TriangulationStore -> [Edge] -> [Edge] -> TriangulationStore
rebuildSiblings store [] _ = store
rebuildSiblings store (edge : es) restrictedEdges =
  let isRestricted = edge `elem` restrictedEdges
      siblingTriangles = getTriangles edge store
      points = getPoints siblingTriangles
      Polygon convexHullPoints = buildConvexHull points
      newTriangles = buildTrianglesOn4Points convexHullPoints points
      firstTriangle = head newTriangles
      (tr1, tr2) = (head siblingTriangles, last siblingTriangles)
  in if isRestricted || length siblingTriangles /= 2 || length convexHullPoints /= 4 || firstTriangle `elem` [tr1, tr2]
     then rebuildSiblings store es restrictedEdges
     else let newEdges = nub $ concatMap getEdges newTriangles ++ es
              store' = foldl (flip deleteTriangle) store [tr1, tr2]
              store'' = foldl (flip insertTriangle) store' newTriangles
          in rebuildSiblings store'' newEdges restrictedEdges

-- rebuilding process
rebuildTriangles :: TriangulationStore -> [Edge] -> [Edge] -> TriangulationStore
rebuildTriangles store [] _ = store
rebuildTriangles store (edge : edges) restrictedEdges =
  let existedEdges = getEdgesFromStore store
      normalizedEdge@(Edge (p1, p2)) = normalizeEdge edge
      maybeCrossedEdges = map (\e -> (e, getIntersectionPoint normalizedEdge e)) existedEdges
      crossedEdges = [(e, fromMaybe p1 mp) | (e, mp) <- maybeCrossedEdges, isJust mp]
      sortedCrossedEdges = sortBy (\(_, a) (_, b) -> compare a b) crossedEdges
      edgesPoints = concatMap (\(Edge (a, b), _) -> [a, b]) sortedCrossedEdges
      pointsLeft = removeSeqDouble $ filter (\p -> calcPointPosition normalizedEdge p == PointLeft) edgesPoints
      pointsRight = removeSeqDouble $ filter (\p -> calcPointPosition normalizedEdge p == PointRight) edgesPoints
      deletingTriangles = nub $ concatMap (\(e, _) -> getTriangles e store) crossedEdges
      store' = foldl (flip deleteTriangle) store deletingTriangles
      store'' = foldr (\points acc -> fillWithTriangles acc points restrictedEdges) store' [p1 : p2 : reverse pointsLeft, p2 : p1 : pointsRight]
  in rebuildTriangles store'' edges restrictedEdges

fillWithTriangles :: TriangulationStore -> [Point] -> [Edge] -> TriangulationStore
fillWithTriangles store [] _ = store
fillWithTriangles store [_] _ = store
fillWithTriangles store [_, _] _ = store
fillWithTriangles store (p1 : p2 : p3 : ps) restrictedEdges
  | checkTriangleCorrectness (Triangle (p1, p2, p3)) ps =
    let triangle = Triangle (p1, p2, p3)
        triangles' = rebuildSiblings (insertTriangle triangle store) (getEdges triangle) restrictedEdges
    in fillWithTriangles triangles' (p1 : p3 : ps) restrictedEdges
  | otherwise = fillWithTriangles store (p2 : p3 : ps ++ [p1]) restrictedEdges

-- checking
checkNotIntersectedConditions :: [Triangle] -> Bool
checkNotIntersectedConditions [] = True
checkNotIntersectedConditions (t:ts) =
  let notCrossedCondition t1 t2 = all (\e1 -> all (isNothing . getIntersectionPoint e1) (getEdges t2)) (getEdges t1)
  in all (notCrossedCondition t) ts && checkNotIntersectedConditions ts

checkDelaunayConditions :: [Triangle] -> Bool
checkDelaunayConditions triangles =
  let points = getPoints triangles
      delaunayCondition triangle = all (`isPointNotInCircle` triangle)
  in all (\triangle@(Triangle (p1, p2, p3)) -> delaunayCondition triangle $ filter (\p -> p `notElem` [p1, p2, p3]) points) triangles

checkTriangleCorrectness :: Triangle -> [Point] -> Bool
checkTriangleCorrectness triangle@(Triangle (p1, p2, p3)) points =
  let noPointsInTriangle = not $ any (isPointInTriangle triangle) points
      allAnglesLess180 = calcPointPosition (Edge (p1, p2)) p3 == PointLeft
  in noPointsInTriangle && allAnglesLess180

isTriangleInPolygon :: Triangle -> Polygon -> Bool
isTriangleInPolygon (Triangle (Point (x1, y1), Point (x2, y2), Point (x3, y3))) polygon =
  let xm = (x1 + x2 + x3) / 3
      ym = (y1 + y2 + y3) / 3
  in isPointInPolygon polygon (Point (xm, ym))

isPointInTriangle :: Triangle -> Point -> Bool
isPointInTriangle (Triangle (point1@(Point (x1, y1)), point2@(Point (x2, y2)), point3@(Point (x3, y3)))) point0@(Point (x0, y0)) =
  let a = (x1 - x0) * (y2 - y1) - (x2 - x1) * (y1 - y0)
      b = (x2 - x0) * (y3 - y2) - (x3 - x2) * (y2 - y0)
      c = (x3 - x0) * (y1 - y3) - (x1 - x3) * (y3 - y0)
  in point0 `notElem` [point1, point2, point3] && ((a >= 0 && b >= 0 && c >= 0) || (a <= 0 && b <= 0 && c <= 0))
