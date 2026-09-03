{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Undirected segments between two points.
--
-- The endpoints are stored in ascending order, so the derived 'Eq', 'Ord' and
-- 'Hashable' instances are lawful and @mkEdge a b == mkEdge b a@. Build edges
-- with 'mkEdge'; take them apart with the read-only v'Edge' pattern.
module Triangulation.Geometry.Edge (
  Edge (Edge),
  mkEdge,
  intersection,
) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable, hashWithSalt)
import GHC.Generics (Generic)
import Triangulation.Geometry.Point (Orientation (..), Point (..), orientation)

-- | An undirected segment; see the module header.
data Edge = UnsafeEdge {-# UNPACK #-} !Point {-# UNPACK #-} !Point
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Written out rather than derived through 'Generic', which dominated the
-- profile. The endpoints are kept sorted, so equal edges hash equally.
instance Hashable Edge where
  hashWithSalt salt (UnsafeEdge a b) = salt `hashWithSalt` a `hashWithSalt` b
  {-# INLINE hashWithSalt #-}

instance Show Edge where
  showsPrec d (UnsafeEdge a b) =
    showParen (d > 10) $ showString "Edge " . showsPrec 11 a . showChar ' ' . showsPrec 11 b

-- | Smart constructor: orders the endpoints.
mkEdge :: Point -> Point -> Edge
mkEdge a b
  | a <= b = UnsafeEdge a b
  | otherwise = UnsafeEdge b a

-- | The endpoints, in ascending order.
pattern Edge :: Point -> Point -> Edge
pattern Edge a b <- UnsafeEdge a b

{-# COMPLETE Edge #-}

-- | The point where two edges cross, if they cross strictly inside both of
-- them: each edge has the other's endpoints strictly on opposite sides.
-- Edges sharing an endpoint, or merely touching one, never count as
-- intersecting. The decision is exact; the returned point is computed in
-- floating point.
intersection :: Edge -> Edge -> Maybe Point
intersection (Edge p1@(Point x1 y1) p2@(Point x2 y2)) (Edge p3@(Point x3 y3) p4@(Point x4 y4))
  | haveCommonPoint = Nothing
  | separates p1 p2 p3 p4 && separates p3 p4 p1 p2 = Just crossing
  | otherwise = Nothing
  where
    haveCommonPoint = p1 == p3 || p1 == p4 || p2 == p3 || p2 == p4
    separates a b c d = case (orientation a b c, orientation a b d) of
      (Clockwise, CounterClockwise) -> True
      (CounterClockwise, Clockwise) -> True
      _ -> False
    det = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3) -- non-zero: the edges are not parallel
    t = ((x3 - x1) * (y4 - y3) - (y3 - y1) * (x4 - x3)) / det
    crossing = Point (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))
