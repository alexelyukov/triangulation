{-# LANGUAGE DerivingStrategies #-}

-- | Deterministic pseudo-random point sets.
module Triangulation.Generator (
  Rectangle (..),
  generatePoints,
  generatePointsWithDistance,
) where

import System.Random (mkStdGen, randoms)
import Triangulation.Geometry.Point (Point (..), manhattanDistance)

-- | An axis-aligned rectangle given by its two extreme corners.
data Rectangle = Rectangle
  { minCorner :: !Point
  -- ^ smallest x and y (lower-left with the y axis up)
  , maxCorner :: !Point
  -- ^ largest x and y (upper-right with the y axis up)
  }
  deriving stock (Eq, Show)

-- | @n@ pseudo-random points in the rectangle, determined by the seed.
generatePoints :: Int -> Int -> Rectangle -> [Point]
generatePoints seed n r = take n $ randomPointsInRectangle seed r

-- | @n@ random points in the rectangle, added to the existing points so that
-- all points are at least @distance@ apart (in 'manhattanDistance').
generatePointsWithDistance :: Int -> Int -> Double -> Rectangle -> [Point] -> [Point]
generatePointsWithDistance seed n distance r =
  addPointsWithDistance n distance (randomPointsInRectangle seed r)

-- | One stream of numbers, taken two at a time: @split@ is deprecated in
-- @random-1.3@ and its replacement does not exist in @random-1.2@.
randomPointsInRectangle :: Int -> Rectangle -> [Point]
randomPointsInRectangle seed r = pairUp (randoms (mkStdGen seed))
  where
    pairUp (x : y : rest) = Point (scaleX r x) (scaleY r y) : pairUp rest
    pairUp _ = []

addPointsWithDistance :: Int -> Double -> [Point] -> [Point] -> [Point]
addPointsWithDistance n distance = go n
  where
    go 0 _ out = out
    go _ [] out = out
    go k (c : cs) out
      | all ((>= distance) . manhattanDistance c) out = go (k - 1) cs (c : out)
      | otherwise = go k cs out

scaleX :: Rectangle -> Double -> Double
scaleX Rectangle {minCorner = Point x1 _, maxCorner = Point x2 _} x = x1 + x * (x2 - x1)

scaleY :: Rectangle -> Double -> Double
scaleY Rectangle {minCorner = Point _ y1, maxCorner = Point _ y2} y = y1 + y * (y2 - y1)
