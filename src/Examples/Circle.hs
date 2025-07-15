module Examples.Circle (
  drawCircle,
  drawTorus
) where

import Geometry.Types.Point ( Point(..) )
import Geometry.Types.Rectangle ( Rectangle(..) )
import Geometry.Types.Polygon ( Polygon(..) )
import Geometry.Types.Edge ( Edge(..) )
import Geometry.Types.Triangle ( Triangle )
import Geometry.Triangle ( rebuildTriangles, isTriangleInPolygon )
import Triangulation ( Triangulation(Triangulation) )
import TriangulationStore ( hasTrianglesOnEdge, getAllTriangles )
import Generator (generatePointsWithDistance)
import TriangulationEvaluation ( buildTEvaluation, evalTriangulation )
import Drawer ( drawTriangulation )

drawCircle :: String -> IO ()
drawCircle path = do
  let circlePoints = getCirclePoints (Point (1000, 1000)) 800 150
      circlePolygon = Polygon circlePoints
      randomPoints = generatePointsWithDistance 2 8000 20 Rectangle { topLeft = Point (0, 0), bottomRight = Point (2000, 2000) } circlePoints
      triangulationTree = buildTEvaluation randomPoints
      (Triangulation _ store) = evalTriangulation triangulationTree
      edges = getCircleEdges circlePolygon
      filtredEdges = filter (\edge -> not $ hasTrianglesOnEdge edge store) edges
      triangles = removeRedundantOuterTriangles circlePolygon (getAllTriangles $ rebuildTriangles store filtredEdges edges)
  drawTriangulation path triangles [] [circlePolygon]

drawTorus :: String -> IO ()
drawTorus path = do
  let outerCirclePoints = getCirclePoints (Point (1000, 1000)) 800 100
      innerCirclePoints = getCirclePoints (Point (1000, 1000)) 400 50
      outerPolygon = Polygon outerCirclePoints
      innerPolygon = Polygon innerCirclePoints
      randomPoints = generatePointsWithDistance 1 4000 20 Rectangle { topLeft = Point (0, 0), bottomRight = Point (2000, 2000) } (outerCirclePoints ++ innerCirclePoints)
      triangulationTree = buildTEvaluation randomPoints
      (Triangulation _ store) = evalTriangulation triangulationTree
      edges = getCircleEdges outerPolygon ++ getCircleEdges innerPolygon
      filtredEdges = filter (\edge -> not $ hasTrianglesOnEdge edge store) edges
      triangles = (removeRedundantOuterTriangles outerPolygon . removeRedundantInnerTriangles innerPolygon) (getAllTriangles $ rebuildTriangles store filtredEdges edges)
  drawTriangulation path triangles [] [outerPolygon, innerPolygon]

getCirclePoints :: Point -> Double -> Int -> [Point]
getCirclePoints (Point (x0, y0)) radius n =
  let angleStep = 2 * pi / fromIntegral n
      angles = [angleStep * fromIntegral i | i <- [0..n-1]]
  in reverse [Point (x0 + radius * cos angle, y0 + radius * sin angle) | angle <- angles]

getCircleEdges :: Polygon -> [Edge]
getCircleEdges (Polygon points) =
  let points' = points ++ [head points]
  in [Edge (points' !! i, points' !! (i + 1)) | i <- [0..length points - 1]]

removeRedundantOuterTriangles :: Polygon -> [Triangle] -> [Triangle]
removeRedundantOuterTriangles polygon = filter (`isTriangleInPolygon` polygon)

removeRedundantInnerTriangles :: Polygon -> [Triangle] -> [Triangle]
removeRedundantInnerTriangles polygon = filter (\t -> not $ isTriangleInPolygon t polygon)
