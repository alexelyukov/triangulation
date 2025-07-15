module Geometry.Types.Polygon (
  Polygon(..)
) where

import Geometry.Types.Point ( Point )
import Control.DeepSeq (NFData, rnf)

newtype Polygon = Polygon [Point]
instance Eq Polygon where
  (Polygon p1) == (Polygon p2) = (length p1 == length p2) && and (zipWith (==) p1 p2)
instance Show Polygon where
  show (Polygon points) = "Polygon " ++ show points
instance NFData Polygon where
  rnf (Polygon points) = rnf points
