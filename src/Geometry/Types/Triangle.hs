module Geometry.Types.Triangle (
  Triangle(..)
) where

import Geometry.Types.Point ( Point )
import Control.DeepSeq (NFData, rnf)

newtype Triangle = Triangle (Point, Point, Point) deriving (Show)

instance NFData Triangle where
    rnf (Triangle (p1, p2, p3)) = rnf p1 `seq` rnf p2 `seq` rnf p3
instance Eq Triangle where
    (Triangle (p1, p2, p3)) == (Triangle (q1, q2, q3)) = p1 == q1 && p2 == q2 && p3 == q3
