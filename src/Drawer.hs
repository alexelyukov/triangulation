module Drawer (
  drawTriangulation
) where

import Graphics.Rasterific (
  renderDrawing,
  withTexture,
  stroke,
  line,
  circle,
  fill,
  Drawing,
  Cap( CapRound ),
  Join( JoinRound ),
  V2(..) )
import Graphics.Rasterific.Texture ( uniformTexture )
import Codec.Picture ( writePng, PixelRGBA8( .. ), Image)
import GHC.Float ( double2Float )
import Geometry.Types.Point ( Point(..) )
import Geometry.Types.Edge ( Edge(..) )
import Geometry.Types.Polygon ( Polygon(..) )
import Geometry.Types.Triangle ( Triangle(..) )

drawTriangulation :: FilePath -> [Triangle] -> [[Point]] -> [Polygon] -> IO ()
drawTriangulation path triangles lines' polygons = do
  writePng path $ drawBackground $ do
    drawLines lines'
    drawPolygons polygons
    drawTriangles triangles

drawBackground :: Drawing PixelRGBA8 () -> Image PixelRGBA8
drawBackground =
  let backgroundColor = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0x00 0x00 0x00 255
      width = 2000
      height = 2000
  in renderDrawing width height backgroundColor . withTexture (uniformTexture drawColor)

drawTriangles :: [Triangle] -> Drawing PixelRGBA8 ()
drawTriangles = mconcat . map drawTriangle

drawTriangle :: Triangle -> Drawing PixelRGBA8 ()
drawTriangle (Triangle (p1, p2, p3)) = do
  drawEdge (Edge (p1, p2))
  drawEdge (Edge (p2, p3))
  drawEdge (Edge (p3, p1))

drawEdge :: Edge -> Drawing PixelRGBA8 ()
drawEdge (Edge (p0, p1)) = do
  stroke 1 JoinRound (CapRound, CapRound) $ line (preparePoint p0) (preparePoint p1)
  drawPoint p0
  drawPoint p1

drawPolygons :: [Polygon] -> Drawing PixelRGBA8 ()
drawPolygons = mconcat . map drawPolygon

drawPolygon :: Polygon -> Drawing PixelRGBA8 ()
drawPolygon (Polygon points) = drawLine (points ++ [head points])

drawLines :: [[Point]] -> Drawing PixelRGBA8 ()
drawLines = mconcat . map drawLine

drawLine :: [Point] -> Drawing PixelRGBA8 ()
drawLine [] = return ()
drawLine [_] = return ()
drawLine (p1:p2:ps) = do
  stroke 3 JoinRound (CapRound, CapRound) $ line (preparePoint p1) (preparePoint p2)
  drawPoint p1
  drawLine (p2:ps)

drawPoint :: Point -> Drawing PixelRGBA8 ()
drawPoint pc =
  let pointColor = PixelRGBA8 0xFF 0x00 0x00 255
  in withTexture (uniformTexture pointColor) $ fill $ circle (preparePoint pc) 3

preparePoint :: Point -> V2 Float
preparePoint (Point (x, y)) = V2 (double2Float x) (double2Float y)
