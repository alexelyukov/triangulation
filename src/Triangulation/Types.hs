{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | The triangulation itself: the convex hull plus the triangles.
module Triangulation.Types (
  Triangulation (..),
  triangles,
  fromTriangles,
  Axis (..),
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Triangulation.Geometry.Polygon (Polygon)
import Triangulation.Geometry.Triangle (Triangle)
import Triangulation.Store (Store)
import Triangulation.Store qualified as Store

-- | A triangulation of a point set.
data Triangulation = Triangulation
  { hull :: !Polygon
  -- ^ the convex hull of the points
  , triangleStore :: !Store
  -- ^ the triangles, indexed by edge
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | All triangles.
triangles :: Triangulation -> [Triangle]
triangles = Store.triangles . triangleStore

-- | Build a triangulation from its hull and triangles.
fromTriangles :: Polygon -> [Triangle] -> Triangulation
fromTriangles polygon = Triangulation polygon . foldr Store.insert Store.empty

-- | A coordinate axis; the direction in which a point set is split before
-- the halves are triangulated and merged.
data Axis = X | Y
  deriving stock (Eq, Show)
