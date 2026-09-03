{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Points in the Euclidean plane and predicates on them: orientation of a
-- triple, angles, distances, extreme points of a set.
--
-- The library uses the mathematical convention: the y axis points up.
-- Rendering code flips it when mapping to pixel rows.
module Triangulation.Geometry.Point (
  Point (..),
  coordinates,
  Orientation (..),
  manhattanDistance,
  orientation,
  turn,
  cosSquaredAngle,
  bottomRight,
  leftTop,
  topLeft,
  rightBottom,
) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftR, xor)
import Data.Foldable (minimumBy)
import Data.Hashable (Hashable, hashWithSalt)
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (Down (..), comparing)
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64)
import GHC.Generics (Generic)
import Triangulation.Geometry.Exact (orientationSign)

-- | A point of the plane.
data Point = Point {px :: !Double, py :: !Double}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A hash of the two coordinates, written out rather than derived.
--
-- 'Data.Hashable's own instance for 'Double' runs a strong mixing function per
-- field, and a profile taken after optimisation showed that mixing to be about
-- half the running time of a triangulation: every lookup in the triangle store
-- hashes points. The geometry only needs a hash good enough to spread keys
-- across a 'Data.HashMap.Strict.HashMap', so the two bit patterns are combined
-- and put through the MurmurHash3 finaliser once. Equal points have equal
-- coordinates, so the instance is lawful.
instance Hashable Point where
  hashWithSalt salt (Point x y) =
    fromIntegral (finalise (castDoubleToWord64 x * goldenGamma `xor` castDoubleToWord64 y))
      `xor` salt
  {-# INLINE hashWithSalt #-}

-- | The odd multiplier of the golden ratio, as used by @splitmix@.
goldenGamma :: Word64
goldenGamma = 0x9E3779B97F4A7C15

-- | The finaliser of MurmurHash3: two multiplications, three shifts, three
-- exclusive ors, and no memory traffic.
finalise :: Word64 -> Word64
finalise w0 =
  let w1 = (w0 `xor` (w0 `shiftR` 33)) * 0xFF51AFD7ED558CCD
      w2 = (w1 `xor` (w1 `shiftR` 33)) * 0xC4CEB9FE1A85EC53
   in w2 `xor` (w2 `shiftR` 33)
{-# INLINE finalise #-}

-- | The direction of the turn made at the middle point of an ordered triple.
data Orientation = Clockwise | CounterClockwise | Collinear
  deriving stock (Eq, Show)

-- | The point as an @(x, y)@ pair, for the exact predicates.
coordinates :: Point -> (Double, Double)
coordinates (Point x y) = (x, y)

-- | Manhattan (L1) distance.
manhattanDistance :: Point -> Point -> Double
manhattanDistance (Point x1 y1) (Point x2 y2) = abs (x2 - x1) + abs (y2 - y1)

-- | Direction of the turn @a -> b -> c@, with the y axis pointing up.
-- Exact: see "Triangulation.Geometry.Exact".
orientation :: Point -> Point -> Point -> Orientation
orientation a b c = case orientationSign (coordinates a) (coordinates b) (coordinates c) of
  LT -> Clockwise
  GT -> CounterClockwise
  EQ -> Collinear

-- | The orientation of the /symbolically perturbed/ points: 'Collinear' only
-- when two of the points coincide.
--
-- This is Edelsbrunner and Mücke's Simulation of Simplicity. Every point is
-- imagined displaced by an infinitesimal amount that depends only on its rank
-- (its position in the 'Ord' order): a lower rank gets a larger displacement,
-- and the y coordinate a larger one than x. Ties in 'orientation' are then
-- broken by the first non-zero term of the perturbed determinant, which for
-- three points ranked @i < j < k@ is, in decreasing significance,
-- @x_k - x_j@, @y_j - y_k@, @x_i - x_k@ and finally a constant. Because the
-- displacement is a fixed function of the points, every decision the
-- algorithm makes is consistent with one and the same perturbed point set,
-- which is what lets the convex-hull and merge code assume general position.
turn :: Point -> Point -> Point -> Orientation
turn a b c = case orientation a b c of
  Collinear
    | a == b || b == c || a == c -> Collinear
    | otherwise -> perturbed
  o -> o
  where
    perturbed =
      let (evenPermutation, Point xi _, Point xj yj, Point xk yk) = rankSorted a b c
          firstNonZero = case filter (/= 0) [xk - xj, yj - yk, xi - xk] of
            t : _ -> compare t 0
            [] -> GT
          sign = if evenPermutation then firstNonZero else flipOrdering firstNonZero
       in case sign of
            GT -> CounterClockwise
            LT -> Clockwise
            EQ -> Collinear
    flipOrdering LT = GT
    flipOrdering GT = LT
    flipOrdering EQ = EQ

-- | The three points in increasing 'Ord' order, and whether that reordering is
-- an even permutation of the arguments.
rankSorted :: Point -> Point -> Point -> (Bool, Point, Point, Point)
rankSorted a b c
  | a <= b && b <= c = (True, a, b, c)
  | a <= c && c <= b = (False, a, c, b)
  | b <= a && a <= c = (False, b, a, c)
  | b <= c && c <= a = (True, b, c, a)
  | c <= a && a <= b = (True, c, a, b)
  | otherwise = (False, c, b, a)

-- | Squared cosine of the angle at @o@ between the rays @o -> a@ and @o -> b@.
cosSquaredAngle :: Point -> Point -> Point -> Double
cosSquaredAngle (Point x0 y0) (Point x1 y1) (Point x2 y2) =
  let (dx1, dy1) = (x1 - x0, y1 - y0)
      (dx2, dy2) = (x2 - x0, y2 - y0)
      dot = dx1 * dx2 + dy1 * dy2
   in dot * dot / ((dx1 * dx1 + dy1 * dy1) * (dx2 * dx2 + dy2 * dy2))

-- | The rightmost of the bottom points.
bottomRight :: NonEmpty Point -> Point
bottomRight = minimumBy (comparing (\(Point x y) -> (y, Down x)))

-- | The topmost of the left points.
leftTop :: NonEmpty Point -> Point
leftTop = minimumBy (comparing (\(Point x y) -> (x, Down y)))

-- | The leftmost of the top points.
topLeft :: NonEmpty Point -> Point
topLeft = minimumBy (comparing (\(Point x y) -> (Down y, x)))

-- | The bottommost of the right points.
rightBottom :: NonEmpty Point -> Point
rightBottom = minimumBy (comparing (\(Point x y) -> (Down x, y)))
