module Geometry.Types.Point (
  Point(..)
) where

import Control.DeepSeq (NFData, rnf)
import Data.Hashable ( Hashable(..), defaultHashWithSalt )

newtype Point = Point (Double, Double)
instance Eq Point where
  (Point (x1, y1)) == (Point (x2, y2)) = x1 == x2 && y1 == y2
instance Show Point where
  show (Point (x, y)) = "(" ++ show x ++ ", " ++ show y ++ ")"
instance NFData Point where
  rnf (Point (x, y)) = rnf x `seq` rnf y
instance Hashable Point where
  hash (Point (x, y)) = hash (x, y)
  hashWithSalt = defaultHashWithSalt
instance Ord Point where
  compare (Point (x1, y1)) (Point (x2, y2)) =
    case compare x1 x2 of
      EQ -> compare y1 y2
      other -> other
