module Geometry.Types.Edge (
  Edge(..),
) where

import Geometry.Types.Point ( Point )
import Data.Hashable ( Hashable(..), defaultHashWithSalt )
import Control.DeepSeq (NFData, rnf)

newtype Edge = Edge (Point, Point) deriving (Show)

instance Eq Edge where
    (Edge (p1, p2)) == (Edge (p3, p4)) = (p1 == p3 && p2 == p4) || (p1 == p4 && p2 == p3)
instance NFData Edge where
    rnf (Edge (p1, p2)) = rnf p1 `seq` rnf p2
instance Hashable Edge where
  hash (Edge (p1, p2)) = hash (p1, p2)
  hashWithSalt = defaultHashWithSalt
