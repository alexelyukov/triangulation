module Examples.Solenoid (
  drawSolenoid
) where

import Geometry.Types.Point ( Point(..) )
import Geometry.Types.Rectangle ( Rectangle(..) )
import Geometry.Types.Triangle ( Triangle )
import Geometry.Types.Edge ( Edge(..) )
import Geometry.Types.Polygon ( Polygon(..) )
import Geometry.Triangle ( rebuildTriangles, isTriangleInPolygon )
import Generator (generatePointsWithDistance)
import Drawer ( drawTriangulation )
import TriangulationStore ( hasTrianglesOnEdge, getAllTriangles )
import TriangulationEvaluation ( buildTEvaluation, evalTriangulation )
import Triangulation ( Triangulation(Triangulation) )

drawSolenoid :: String -> IO ()
drawSolenoid path = do
  let outerRectangle = getRectanglePoints Rectangle { topLeft = Point (100, 200), bottomRight = Point (1900, 1800) } (30, 20)
      innerRectangleLeft = getRectanglePoints Rectangle { topLeft = Point (300, 400), bottomRight = Point (900, 1600) } (10, 20)
      innerRectangleRight = getRectanglePoints Rectangle { topLeft = Point (1100, 400), bottomRight = Point (1700, 1600) }  (10, 20)
      outerRectanglePolygon = Polygon outerRectangle
      innerRectangleLeftPolygon = Polygon innerRectangleLeft
      innerRectangleRightPolygon = Polygon innerRectangleRight
      randomPoints = generatePointsWithDistance 1 4000 20 Rectangle { topLeft = Point (0, 0), bottomRight = Point (2000, 2000) }
        (outerRectangle ++ innerRectangleLeft ++ innerRectangleRight)
      triangulationTree = buildTEvaluation randomPoints
      (Triangulation _ store) = evalTriangulation triangulationTree
      edges = getRectangleEdges outerRectanglePolygon
              ++ getRectangleEdges innerRectangleLeftPolygon
              ++ getRectangleEdges innerRectangleRightPolygon
      filtredEdges = filter (\edge -> not $ hasTrianglesOnEdge edge store) edges
      triangles = (removeRedundantOuterTriangles outerRectanglePolygon .
                   removeRedundantInnerTriangles innerRectangleLeftPolygon .
                   removeRedundantInnerTriangles innerRectangleRightPolygon)
                  (getAllTriangles $ rebuildTriangles store filtredEdges edges)
  drawTriangulation path triangles [] [outerRectanglePolygon, innerRectangleLeftPolygon, innerRectangleRightPolygon]

getRectanglePoints :: Rectangle -> (Int, Int) -> [Point]
getRectanglePoints Rectangle { topLeft = Point (x1, y1), bottomRight = Point (x2, y2) } (numH, numV) =
  let pLeftTop = Point (x1, y1)
      pRightTop = Point (x2, y1)
      pRightBottom = Point (x2, y2)
      pLeftBottom = Point (x1, y2)
      lineRight = (pRightBottom : getLinePointsWithoutEndpoints pRightBottom pRightTop numV) ++ [pRightTop]
      lineTop = getLinePointsWithoutEndpoints pRightTop pLeftTop numH
      lineLeft = (pLeftTop : getLinePointsWithoutEndpoints pLeftTop pLeftBottom numV) ++ [pLeftBottom]
      lineBottom = getLinePointsWithoutEndpoints pLeftBottom pRightBottom numH
  in lineRight ++ lineTop ++ lineLeft ++ lineBottom

getLinePointsWithoutEndpoints :: Point -> Point -> Int -> [Point]
getLinePointsWithoutEndpoints (Point (x1, y1)) (Point (x2, y2)) n =
  let dx = (x2 - x1) / fromIntegral (n + 1)
      dy = (y2 - y1) / fromIntegral (n + 1)
  in [ Point (x1 + dx * fromIntegral i, y1 + dy * fromIntegral i) | i <- [1..n] ]

getRectangleEdges :: Polygon -> [Edge]
getRectangleEdges (Polygon points) =
  let points' = points ++ [head points]
  in [Edge (points' !! i, points' !! (i + 1)) | i <- [0..length points - 1]]

removeRedundantOuterTriangles :: Polygon -> [Triangle] -> [Triangle]
removeRedundantOuterTriangles polygon = filter (`isTriangleInPolygon` polygon)

removeRedundantInnerTriangles :: Polygon -> [Triangle] -> [Triangle]
removeRedundantInnerTriangles polygon = filter (\t -> not $ isTriangleInPolygon t polygon)
