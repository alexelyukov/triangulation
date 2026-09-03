-- | Exact geometric predicates on 'Double' coordinates.
--
-- Each predicate is the sign of a determinant. The determinant is first
-- evaluated in floating point together with a bound on its rounding error
-- (the static filters of Shewchuk's /Adaptive Precision Floating-Point
-- Arithmetic and Fast Robust Geometric Predicates/); when the magnitude of the
-- result exceeds the bound, its sign is certain. Otherwise the determinant is
-- recomputed in 'Rational' arithmetic, which is exact because every 'Double'
-- converts to a 'Rational' without loss. The exact branch is a lazy thunk and
-- is only evaluated in the rare near-degenerate cases.
--
-- The predicates take raw coordinates so that this module sits below the
-- geometric types.
module Triangulation.Geometry.Exact (
  Coordinates,
  orientationSign,
  inCircleSign,
) where

-- | A point as an @(x, y)@ pair.
type Coordinates = (Double, Double)

-- | Half the machine epsilon of 'Double': the relative rounding error of one operation.
epsilon :: Double
epsilon = encodeFloat 1 (-53)

-- | Sign of @(a - c) × (b - c)@: 'GT' when @a -> b -> c@ turns counter-clockwise
-- (y axis up), 'LT' when clockwise, 'EQ' when the points are collinear.
--
-- The exact fallback is a separate function rather than an argument, so that
-- the common case does not build a 'Rational' thunk it never looks at.
orientationSign :: Coordinates -> Coordinates -> Coordinates -> Ordering
orientationSign a@(ax, ay) b@(bx, by) c@(cx, cy)
  | approximate > errorBound = GT
  | approximate < negate errorBound = LT
  | otherwise = exactOrientation a b c
  where
    detLeft = (ax - cx) * (by - cy)
    detRight = (ay - cy) * (bx - cx)
    approximate = detLeft - detRight
    errorBound = (3 + 16 * epsilon) * epsilon * (abs detLeft + abs detRight)

-- | The orientation determinant in exact arithmetic. Every 'Double' converts
-- to a 'Rational' without loss, so the sign is the true one.
exactOrientation :: Coordinates -> Coordinates -> Coordinates -> Ordering
{-# NOINLINE exactOrientation #-}
exactOrientation (ax, ay) (bx, by) (cx, cy) =
  compare ((r ax - r cx) * (r by - r cy) - (r ay - r cy) * (r bx - r cx)) 0
  where
    r = toRational

-- | Sign of the in-circle determinant: for @a@, @b@, @c@ in counter-clockwise
-- order, 'GT' when @d@ lies strictly inside their circumcircle, 'LT' when
-- strictly outside, 'EQ' when on it. The sign flips for a clockwise triple.
inCircleSign :: Coordinates -> Coordinates -> Coordinates -> Coordinates -> Ordering
inCircleSign a@(ax, ay) b@(bx, by) c@(cx, cy) d@(dx, dy)
  | det > errorBound = GT
  | det < negate errorBound = LT
  | otherwise = exactInCircle a b c d
  where
    (adx, ady) = (ax - dx, ay - dy)
    (bdx, bdy) = (bx - dx, by - dy)
    (cdx, cdy) = (cx - dx, cy - dy)
    (bdxcdy, cdxbdy, alift) = (bdx * cdy, cdx * bdy, adx * adx + ady * ady)
    (cdxady, adxcdy, blift) = (cdx * ady, adx * cdy, bdx * bdx + bdy * bdy)
    (adxbdy, bdxady, clift) = (adx * bdy, bdx * ady, cdx * cdx + cdy * cdy)
    det = alift * (bdxcdy - cdxbdy) + blift * (cdxady - adxcdy) + clift * (adxbdy - bdxady)
    permanent =
      (abs bdxcdy + abs cdxbdy) * alift
        + (abs cdxady + abs adxcdy) * blift
        + (abs adxbdy + abs bdxady) * clift
    errorBound = (10 + 96 * epsilon) * epsilon * permanent

-- | The in-circle determinant in exact arithmetic; see 'exactOrientation'.
exactInCircle :: Coordinates -> Coordinates -> Coordinates -> Coordinates -> Ordering
{-# NOINLINE exactInCircle #-}
exactInCircle (ax, ay) (bx, by) (cx, cy) (dx, dy) = compare determinant 0
  where
    (ax', ay', bx', by', cx', cy', dx', dy') = (r ax, r ay, r bx, r by, r cx, r cy, r dx, r dy)
    (adx, ady) = (ax' - dx', ay' - dy')
    (bdx, bdy) = (bx' - dx', by' - dy')
    (cdx, cdy) = (cx' - dx', cy' - dy')
    determinant =
      (adx * adx + ady * ady) * (bdx * cdy - cdx * bdy)
        + (bdx * bdx + bdy * bdy) * (cdx * ady - adx * cdy)
        + (cdx * cdx + cdy * cdy) * (adx * bdy - bdx * ady)
    r = toRational
