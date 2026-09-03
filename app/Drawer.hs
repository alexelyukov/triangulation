-- | PNG rendering of triangulations. The library's y axis points up; here it
-- is flipped to pixel rows.
module Drawer (
  drawTriangulation,
) where

import Codec.Picture (Image, PixelRGBA8 (..), writePng)
import Data.Foldable (traverse_)
import Graphics.Rasterific (
  Cap (CapRound),
  Drawing,
  Join (JoinRound),
  V2 (..),
  circle,
  fill,
  line,
  renderDrawing,
  stroke,
  withTexture,
 )
import Graphics.Rasterific.Texture (uniformTexture)
import Triangulation.Geometry (
  Edge (..),
  Point (..),
  Polygon (..),
  Triangle (..),
  cyclicPairs,
  mkEdge,
 )

drawTriangulation :: FilePath -> [Triangle] -> [[Point]] -> [Polygon] -> IO ()
drawTriangulation path triangles polylines polygons =
  writePng path $ drawBackground $ do
    traverse_ drawPolyline polylines
    traverse_ drawPolygon polygons
    traverse_ drawTriangle triangles

-- | Side of the square canvas, in pixels.
canvasSize :: Int
canvasSize = 2000

drawBackground :: Drawing PixelRGBA8 () -> Image PixelRGBA8
drawBackground =
  let backgroundColor = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0x00 0x00 0x00 255
   in renderDrawing canvasSize canvasSize backgroundColor . withTexture (uniformTexture drawColor)

drawTriangle :: Triangle -> Drawing PixelRGBA8 ()
drawTriangle (Triangle p1 p2 p3) = traverse_ drawEdge [mkEdge p1 p2, mkEdge p2 p3, mkEdge p3 p1]

drawEdge :: Edge -> Drawing PixelRGBA8 ()
drawEdge (Edge p0 p1) = do
  drawSegment 1 (p0, p1)
  drawPoint p0
  drawPoint p1

drawPolygon :: Polygon -> Drawing PixelRGBA8 ()
drawPolygon (Polygon points) = do
  traverse_ (drawSegment 3) (cyclicPairs points)
  traverse_ drawPoint points

drawPolyline :: [Point] -> Drawing PixelRGBA8 ()
drawPolyline points = do
  traverse_ (drawSegment 3) (zip points (drop 1 points))
  traverse_ drawPoint points

drawSegment :: Float -> (Point, Point) -> Drawing PixelRGBA8 ()
drawSegment width (p0, p1) =
  stroke width JoinRound (CapRound, CapRound) $ line (preparePoint p0) (preparePoint p1)

drawPoint :: Point -> Drawing PixelRGBA8 ()
drawPoint pc =
  let pointColor = PixelRGBA8 0xFF 0x00 0x00 255
   in withTexture (uniformTexture pointColor) $ fill $ circle (preparePoint pc) 3

-- | Map a library point (y up) to a pixel position (y down).
preparePoint :: Point -> V2 Float
preparePoint (Point x y) = V2 (realToFrac x) (realToFrac (fromIntegral canvasSize - y))
