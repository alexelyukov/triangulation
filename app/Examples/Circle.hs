module Examples.Circle (
  drawCircle,
  drawTorus,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Drawer (drawTriangulation)
import Examples.Common (canvas, orFail)
import Triangulation (Point (..), Polygon (..), constrainedTriangulate, vertices)
import Triangulation.Generator (generatePointsWithDistance)

drawCircle :: FilePath -> IO ()
drawCircle path = do
  triangles <- orFail $ constrainedTriangulate circle [] points
  drawTriangulation path triangles [] [circle]
  where
    circle = circleOf (Point 1000 1000) 800 150
    points = generatePointsWithDistance 2 8000 20 canvas (vertices circle)

drawTorus :: FilePath -> IO ()
drawTorus path = do
  triangles <- orFail $ constrainedTriangulate outer [inner] points
  drawTriangulation path triangles [] [outer, inner]
  where
    outer = circleOf (Point 1000 1000) 800 100
    inner = circleOf (Point 1000 1000) 400 50
    points = generatePointsWithDistance 1 4000 20 canvas (vertices outer ++ vertices inner)

-- | A regular @n@-gon inscribed in the circle, wound clockwise.
circleOf :: Point -> Double -> Int -> Polygon
circleOf (Point x0 y0) radius n =
  let angleStep = 2 * pi / fromIntegral n
      angles = NE.map ((angleStep *) . fromIntegral) (0 :| [1 .. n - 1])
   in Polygon
        $ NE.reverse
        $ NE.map (\angle -> Point (x0 + radius * cos angle) (y0 + radius * sin angle)) angles
