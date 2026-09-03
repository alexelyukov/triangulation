module Examples.Solenoid (
  drawSolenoid,
  drawRefinedSolenoid,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Drawer (drawTriangulation)
import Examples.Common (canvas, orFail)
import Triangulation (
  Point (..),
  Polygon (..),
  Quality (..),
  constrainedTriangulate,
  defaultQuality,
  refine,
  vertices,
 )
import Triangulation.Generator (Rectangle (..), generatePointsWithDistance)

drawSolenoid :: FilePath -> IO ()
drawSolenoid path = do
  triangles <- orFail $ constrainedTriangulate outer [innerLeft, innerRight] points
  drawTriangulation path triangles [] [outer, innerLeft, innerRight]
  where
    outer = rectangleOf Rectangle {minCorner = Point 100 200, maxCorner = Point 1900 1800} (30, 20)
    innerLeft = rectangleOf Rectangle {minCorner = Point 300 400, maxCorner = Point 900 1600} (10, 20)
    innerRight = rectangleOf Rectangle {minCorner = Point 1100 400, maxCorner = Point 1700 1600} (10, 20)
    points = generatePointsWithDistance 1 4000 20 canvas (concatMap vertices [outer, innerLeft, innerRight])

-- | The boundary of a rectangle as a clockwise polygon, with @numH@ extra
-- points on each horizontal side and @numV@ on each vertical side.
rectangleOf :: Rectangle -> (Int, Int) -> Polygon
rectangleOf Rectangle {minCorner = Point x1 y1, maxCorner = Point x2 y2} (numH, numV) =
  let lowerLeft = Point x1 y1
      lowerRight = Point x2 y1
      upperRight = Point x2 y2
      upperLeft = Point x1 y2
      sideRight = pointsBetween upperRight lowerRight numV ++ [lowerRight]
      sideBottom = pointsBetween lowerRight lowerLeft numH ++ [lowerLeft]
      sideLeft = pointsBetween lowerLeft upperLeft numV ++ [upperLeft]
      sideTop = pointsBetween upperLeft upperRight numH
   in Polygon (upperRight :| (sideRight ++ sideBottom ++ sideLeft ++ sideTop))

-- | @n@ equally spaced points strictly between two points.
pointsBetween :: Point -> Point -> Int -> [Point]
pointsBetween (Point x1 y1) (Point x2 y2) n =
  let dx = (x2 - x1) / fromIntegral (n + 1)
      dy = (y2 - y1) / fromIntegral (n + 1)
   in [Point (x1 + dx * fromIntegral i) (y1 + dy * fromIntegral i) | i <- [1 .. n]]

-- | The same section, meshed for finite elements: the polygons with only a
-- few points on each side, refined to a 28° angle bound and a maximum
-- triangle area, so that the density comes from the refinement rather than
-- from random points.
drawRefinedSolenoid :: FilePath -> IO ()
drawRefinedSolenoid path = do
  coarse <- orFail $ constrainedTriangulate outer [innerLeft, innerRight] []
  triangles <- orFail $ refine defaultQuality {minAngle = 28, maxArea = Just 1500} coarse
  drawTriangulation path triangles [] [outer, innerLeft, innerRight]
  where
    outer = rectangleOf Rectangle {minCorner = Point 100 200, maxCorner = Point 1900 1800} (3, 2)
    innerLeft = rectangleOf Rectangle {minCorner = Point 300 400, maxCorner = Point 900 1600} (1, 2)
    innerRight = rectangleOf Rectangle {minCorner = Point 1100 400, maxCorner = Point 1700 1600} (1, 2)
