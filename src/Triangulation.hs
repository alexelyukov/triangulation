module Triangulation (
  Triangulation(..),
  triangulate,
  mergeTriangulations,
  checkDelaunay,
  checkNotIntersected
) where

import Control.DeepSeq (NFData, rnf)
import Geometry.Types.Partition ( Partition(..) )
import Geometry.Types.Point ( Point )
import Geometry.Types.Polygon ( Polygon(..) )
import Geometry.Types.Edge ( Edge(..) )
import Geometry.Polygon ( buildConvexHull, calcTangents, mergePolygons )
import Geometry.Triangle (
  mergeTriangles,
  buildTrianglesOn3Points,
  buildTrianglesOn4Points,
  checkDelaunayConditions,
  checkNotIntersectedConditions
  )
import Geometry.Point ( getRightBottomPoint, getBottomRightPoint, getLeftTopPoint, getTopLeftPoint )
import TriangulationStore (
  TriangulationStore,
  insertTriangle,
  emptyTriangulationStore,
  getAllTriangles,
  unionTriangulationStores
  )
import Utils ( rebuildBy2Elements )

data Triangulation = Triangulation Polygon TriangulationStore deriving (Show)

instance NFData Triangulation where
  rnf (Triangulation poly newTris) = rnf poly `seq` rnf newTris

triangulate :: [Point] -> Triangulation
triangulate points =
  let convexHull = buildConvexHull points
      Polygon convexHullPoints = convexHull
      triangles = if length points == 3
                  then buildTrianglesOn3Points convexHullPoints
                  else buildTrianglesOn4Points convexHullPoints points
      triangles' = foldr insertTriangle emptyTriangulationStore triangles
  in Triangulation convexHull triangles'

mergeTriangulations :: Triangulation -> Triangulation -> Partition -> Triangulation
mergeTriangulations (Triangulation polygon1@(Polygon points1) triangles1) (Triangulation polygon2@(Polygon points2) triangles2) partition =
  let edgePointLeft = (if partition == Vertical then getRightBottomPoint else getBottomRightPoint) points1
      edgePointRight = (if partition == Vertical then getLeftTopPoint else getTopLeftPoint) points2
      (edgeBottom, edgeTop) = (Edge (edgePointLeft, edgePointRight), Edge (edgePointLeft, edgePointRight))
      mergedPolygon = mergePolygons polygon1 polygon2 (edgeBottom, edgeTop)
      (Edge (bl, br), Edge (tl, tr), mergedPolygon') = calcTangents polygon1 polygon2 (edgeBottom, edgeTop, mergedPolygon)
      remainsPolygon1 = rebuildBy2Elements points1 bl tl
      remainsPolygon2 = reverse $ rebuildBy2Elements points2 tr br
      unitedTriangles = unionTriangulationStores triangles1 triangles2
      mergedTriangles = mergeTriangles unitedTriangles remainsPolygon1 remainsPolygon2 []
  in Triangulation mergedPolygon' mergedTriangles

checkDelaunay :: Triangulation -> Bool
checkDelaunay (Triangulation _ store) = checkDelaunayConditions $ getAllTriangles store

checkNotIntersected :: Triangulation -> Bool
checkNotIntersected (Triangulation _ store) = checkNotIntersectedConditions $ getAllTriangles store
