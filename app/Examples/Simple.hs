module Examples.Simple (
  drawSimple,
) where

import Drawer (drawTriangulation)
import Examples.Common (canvas, orFail)
import Triangulation (triangles, triangulate)
import Triangulation.Generator (generatePoints)

drawSimple :: FilePath -> IO ()
drawSimple path = do
  triangulation <- orFail $ triangulate (generatePoints 2 8 canvas)
  drawTriangulation path (triangles triangulation) [] []
