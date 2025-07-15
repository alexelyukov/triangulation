module Examples.Simple (
  drawSimple
) where

import Geometry.Types.Point ( Point(..) )
import Geometry.Types.Rectangle ( Rectangle(..) )
import Generator (generatePoints)
import Triangulation ( Triangulation(..) )
import TriangulationEvaluation ( buildTEvaluation, evalTriangulation )
import TriangulationStore ( getAllTriangles )
import Drawer ( drawTriangulation )

drawSimple :: String -> IO ()
drawSimple path = do
  let randomPoints = generatePoints 2 8 Rectangle { topLeft = Point (0, 0), bottomRight = Point (2000, 2000) }
      triangulationTree = buildTEvaluation randomPoints
      (Triangulation _ trianglesHM) = evalTriangulation triangulationTree
      triangles = getAllTriangles trianglesHM
  drawTriangulation path triangles [] []

-- twoVerticalLines :: [Point]
-- twoVerticalLines = [
  -- Point (900, 700), Point (900, 900), Point (900, 1100), Point (900, 1300),
  -- Point (1100, 700), Point (1100, 900), Point (1100, 1100), Point (1100, 1300)
  -- ]

-- twoHorizontalLines :: [Point]
-- twoHorizontalLines = [
  -- Point (700, 900), Point (900, 900), Point (1100, 900), Point (1300, 900),
  -- Point (700, 1100), Point (900, 1100), Point (1100, 1100), Point (1300, 1100)
  -- ]
