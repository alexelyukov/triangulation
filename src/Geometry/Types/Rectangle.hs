module Geometry.Types.Rectangle (
  Rectangle(..)
) where

import Geometry.Types.Point ( Point )

data Rectangle = Rectangle { topLeft :: Point, bottomRight :: Point }
