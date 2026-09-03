-- | A gear with a keyed bore, meshed for finite elements: the outline and the
-- hole are the only input, the density comes from the refinement.
module Examples.Gear (
  drawGear,
) where

import Data.List.NonEmpty qualified as NE
import Drawer (drawTriangulation)
import Examples.Common (orFail)
import Triangulation (
  Point (..),
  Polygon (..),
  Quality (..),
  constrainedTriangulate,
  defaultQuality,
  refine,
 )

drawGear :: FilePath -> IO ()
drawGear path = do
  coarse <- orFail $ constrainedTriangulate gear [bore] []
  triangles <- orFail $ refine defaultQuality {minAngle = 28, maxArea = Just 1200} coarse
  drawTriangulation path triangles [] [gear, bore]

centre :: Point
centre = Point 1000 1000

-- | The outline: 16 trapezoidal teeth on a root circle.
gear :: Polygon
gear = clockwise (concatMap tooth [0 .. teeth - 1])
  where
    teeth = 16 :: Int
    (root, tip) = (700, 850)
    pitch = 2 * pi / fromIntegral teeth
    tooth i =
      let a0 = fromIntegral i * pitch
          at = polar
       in [ at root a0
          , at root (a0 + 0.1 * pitch)
          , at root (a0 + 0.2 * pitch)
          , at tip (a0 + 0.3 * pitch)
          , at tip (a0 + 0.4 * pitch)
          , at tip (a0 + 0.5 * pitch)
          , at tip (a0 + 0.6 * pitch)
          , at tip (a0 + 0.7 * pitch)
          , at root (a0 + 0.8 * pitch)
          , at root (a0 + 0.9 * pitch)
          ]

-- | The hole: a circle with a rectangular keyway cut into it at the top.
bore :: Polygon
bore = clockwise (concatMap arcPoint [0 .. n - 1])
  where
    n = 48 :: Int
    radius = 260
    (halfWidth, depth) = (45, 320)
    step = 2 * pi / fromIntegral n
    keywayEdge = asin (halfWidth / radius) -- half-angle of the keyway opening
    arcPoint i
      | a > pi / 2 - keywayEdge && a < pi / 2 + keywayEdge = []
      | a <= pi / 2 && a + step > pi / 2 - keywayEdge =
          [ polar radius a
          , Point (1000 + halfWidth) (1000 + rim)
          , Point (1000 + halfWidth) (1000 + depth)
          , Point (1000 - halfWidth) (1000 + depth)
          , Point (1000 - halfWidth) (1000 + rim)
          ]
      | otherwise = [polar radius a]
      where
        a = fromIntegral i * step
    rim = sqrt (radius * radius - halfWidth * halfWidth)

-- | A point at the given radius and angle around the centre.
polar :: Double -> Double -> Point
polar r a = Point (px centre + r * cos a) (py centre + r * sin a)

-- | Points listed counter-clockwise (increasing angle) as a clockwise polygon.
clockwise :: [Point] -> Polygon
clockwise = Polygon . NE.reverse . NE.fromList
