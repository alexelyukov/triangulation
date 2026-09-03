-- | Shared pieces of the examples.
module Examples.Common (
  canvas,
  orFail,
) where

import Triangulation.Generator (Rectangle (..))
import Triangulation.Geometry (Point (..))

-- | The area random points are drawn from; matches the canvas of "Drawer".
canvas :: Rectangle
canvas = Rectangle {minCorner = Point 0 0, maxCorner = Point 2000 2000}

-- | Unwrap a triangulation result, aborting the program if it failed.
orFail :: Maybe a -> IO a
orFail = maybe (fail "triangulation failed: fewer than three points or inconsistent hulls") pure
