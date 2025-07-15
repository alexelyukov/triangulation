module Geometry.Polygon (
  buildConvexHull,
  mergePolygons,
  calcTangents,
  isConvexHull,
  isPointInPolygon
) where

import Data.List (sortBy)
import Geometry.Types.Point ( Point(..) )
import Geometry.Types.Edge ( Edge(..) )
import Geometry.Types.Polygon ( Polygon(..) )
import Geometry.Types.PointPosition ( PointPosition(..) )
import Geometry.Point ( getLeftBottomPoint )
import Geometry.Edge ( calcPointPosition )
import Utils (
  splitByElement,
  rebuildBy2Elements,
  predOfFirstElement,
  predOfLastElement,
  succOfFirstElement,
  succOfLastElement
  )

-- convex hull
buildConvexHull :: [Point] -> Polygon
buildConvexHull [p1, p2, p3] =
  let sp1 = getLeftBottomPoint [p1, p2, p3]
      otherPoints = filter (/= sp1) [p1, p2, p3]
      sortedPoints = sortBy (\a b -> pointPosition2Ordering $ calcPointPosition (Edge(sp1, a)) b) otherPoints
  in Polygon (sp1 : sortedPoints)
buildConvexHull points@[_, _, _, _] =
  let sp1 = getLeftBottomPoint points
      otherPoints = filter (/= sp1) points
      otherPointsSorted = sortBy (\a b -> pointPosition2Ordering $ calcPointPosition (Edge(sp1, a)) b) otherPoints
      (sp2, sp3, sp4) = (otherPointsSorted !! 0, otherPointsSorted !! 1, otherPointsSorted !! 2)
      ordering234 = calcPointPosition (Edge (sp2, sp3)) sp4
      ordering341 = calcPointPosition (Edge (sp3, sp4)) sp1
  in buildPolygonOn4Points (sp1 : otherPointsSorted) ordering234 ordering341
buildConvexHull _ = error "Invalid number of points for convex hull"

pointPosition2Ordering :: PointPosition -> Ordering
pointPosition2Ordering PointLeft = LT
pointPosition2Ordering PointRight = GT

buildPolygonOn4Points :: [Point] -> PointPosition -> PointPosition -> Polygon
buildPolygonOn4Points [p1, p2, _, p4] PointRight _ = Polygon [p1, p2, p4]
buildPolygonOn4Points [p1, p2, p3, p4] _ PointLeft = Polygon [p1, p2, p3, p4]
buildPolygonOn4Points [p1, p2, p3, _] _ _ = Polygon [p1, p2, p3] -- rare case for points on the same line
buildPolygonOn4Points _ _ _ = error "Invalid points for polygon"

-- tangents
mergePolygons :: Polygon -> Polygon -> (Edge, Edge) -> Polygon
mergePolygons (Polygon points1) (Polygon points2) (Edge (bl, br), Edge (tl, tr)) =
  let cutPoints1 = rebuildBy2Elements points1 tl bl -- left/top polygon
      cutPoints2 = rebuildBy2Elements points2 br tr -- right/bottom polygon
  in Polygon $ cutPoints1 ++ cutPoints2

calcTangents :: Polygon -> Polygon -> (Edge, Edge, Polygon) -> (Edge, Edge, Polygon)
calcTangents polygon1@(Polygon points1) polygon2@(Polygon points2) (edge1@(Edge (bl, br)), edge2@(Edge (tl, tr)), polygon@(Polygon points)) =
  let blPointPosition = calcTangentPointPosition (predOfLastElement points) (succOfLastElement points) bl
      brPointPosition = calcTangentPointPosition (predOfFirstElement points) (succOfFirstElement points) br
      tlPointPosition = calcTangentPointPosition (predOfFirstElement points) (succOfFirstElement points) tl
      trPointPosition = calcTangentPointPosition (predOfLastElement points) (succOfLastElement points) tr
  in case calcTangentEdges (blPointPosition, brPointPosition, tlPointPosition, trPointPosition) (edge1, edge2) points1 points2 of
    Just (newEdge1, newEdge2) ->
      let mergedPolygon = mergePolygons polygon1 polygon2 (newEdge1, newEdge2)
      in calcTangents polygon1 polygon2 (newEdge1, newEdge2, mergedPolygon)
    Nothing ->
      let newPolygon = removeLoops polygon (edge1, edge2)
      in (edge1, edge2, newPolygon)

calcTangentPointPosition :: (Point -> Point) -> (Point -> Point) -> Point -> PointPosition
calcTangentPointPosition predFn succFn point = calcPointPosition (Edge (predFn point, point)) (succFn point)

calcTangentEdges :: (PointPosition, PointPosition, PointPosition, PointPosition) -> (Edge, Edge) -> [Point] -> [Point] -> Maybe (Edge, Edge)
calcTangentEdges (PointRight, _, _, _) (Edge (bl, br), edge) points1 _ = Just (Edge (predOfLastElement points1 bl, br), edge)
calcTangentEdges (_, PointRight, _, _) (Edge (bl, br), edge) _ points2 = Just (Edge (bl, succOfFirstElement points2 br), edge)
calcTangentEdges (_, _, PointRight, _) (edge, Edge (tl, tr)) points1 _ = Just (edge, Edge (succOfFirstElement points1 tl, tr))
calcTangentEdges (_, _, _, PointRight) (edge, Edge (tl, tr)) _ points2 = Just (edge, Edge (tl, predOfLastElement points2 tr))
calcTangentEdges _ _ _ _ = Nothing

removeLoops :: Polygon -> (Edge, Edge) -> Polygon
removeLoops polygon (Edge (bl, br), Edge (tl, tr))
  | bl == tl = calcTangentPolygon polygon bl
  | br == tr = calcTangentPolygon polygon br
  | otherwise = polygon

calcTangentPolygon :: Polygon -> Point -> Polygon
calcTangentPolygon (Polygon points) point =
  let (points1, points2) = splitByElement points point
      square1 = getPolygonCapacity (Polygon points1)
      square2 = getPolygonCapacity (Polygon points2)
  in if square1 > square2 then Polygon points1 else Polygon points2

getPolygonCapacity :: Polygon -> Double
getPolygonCapacity (Polygon points) =
  let points' = tail points ++ [head points]
  in abs $ sum [x1 * y2 - y1 * x2 | (Point (x1, y1), Point (x2, y2)) <- zip points points']

-- checking
isConvexHull :: Polygon -> Bool
isConvexHull (Polygon [_, _]) = True
isConvexHull (Polygon points) =
  let points' = tail points ++ [head points]
      points'' = tail points' ++ [head points']
  in all (\(p1, p2, p3) -> calcPointPosition (Edge (p1, p2)) p3 /= PointRight) $ zip3 points points' points''

isPointInPolygon :: Polygon -> Point -> Bool
isPointInPolygon (Polygon points) point =
  let edges = zip points (tail points ++ [head points])
      isInside = all (\(p1, p2) -> calcPointPosition (Edge (p1, p2)) point == PointLeft) edges
  in point `notElem` points && isInside
