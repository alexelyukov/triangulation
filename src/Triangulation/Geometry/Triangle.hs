{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Triangles and predicates on them: the Delaunay in-circle test, point
-- containment, position relative to a polygon.
--
-- Vertices are stored in ascending order, so two triangles on the same three
-- points are equal regardless of the order they were built from, and the
-- derived 'Eq', 'Ord' and 'Hashable' instances are lawful. Build triangles
-- with 'mkTriangle'; take them apart with the read-only v'Triangle' pattern.
module Triangulation.Geometry.Triangle (
  Triangle (Triangle),
  mkTriangle,
  triangleEdges,
  triangleArea,
  smallestAngle,
  circumcenter,
  isOutsideCircumcircle,
  isPointInTriangle,
  isValidCandidate,
  isTriangleInPolygon,
  trianglesInside,
  trianglesOutside,
) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable, hashWithSalt)
import Data.List (sort)
import GHC.Generics (Generic)
import Triangulation.Geometry.Edge (Edge, mkEdge)
import Triangulation.Geometry.Exact (inCircleSign)
import Triangulation.Geometry.Point (Orientation (..), Point (..), coordinates, orientation, turn)
import Triangulation.Geometry.Polygon (Polygon, isPointInPolygon)

-- | A triangle; see the module header.
data Triangle = UnsafeTriangle {-# UNPACK #-} !Point {-# UNPACK #-} !Point {-# UNPACK #-} !Point
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Written out rather than derived through 'Generic', which dominated the
-- profile. The vertices are kept sorted, so equal triangles hash equally.
instance Hashable Triangle where
  hashWithSalt salt (UnsafeTriangle a b c) =
    salt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
  {-# INLINE hashWithSalt #-}

instance Show Triangle where
  showsPrec d (UnsafeTriangle a b c) =
    showParen (d > 10) $
      showString "Triangle "
        . showsPrec 11 a
        . showChar ' '
        . showsPrec 11 b
        . showChar ' '
        . showsPrec 11 c

-- | Smart constructor: sorts the vertices.
mkTriangle :: Point -> Point -> Point -> Triangle
mkTriangle a b c
  | c <= lo = UnsafeTriangle c lo hi
  | c <= hi = UnsafeTriangle lo c hi
  | otherwise = UnsafeTriangle lo hi c
  where
    lo = min a b
    hi = max a b

-- | The vertices, in ascending order.
pattern Triangle :: Point -> Point -> Point -> Triangle
pattern Triangle a b c <- UnsafeTriangle a b c

{-# COMPLETE Triangle #-}

-- | The three edges.
triangleEdges :: Triangle -> [Edge]
triangleEdges (Triangle p1 p2 p3) = [mkEdge p1 p2, mkEdge p2 p3, mkEdge p3 p1]

-- | The (unsigned) area.
triangleArea :: Triangle -> Double
triangleArea (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) =
  abs ((x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)) / 2

-- | The smallest of the three angles, in degrees; 0 for a flat triangle.
smallestAngle :: Triangle -> Double
smallestAngle (Triangle a b c)
  | orientation a b c == Collinear = 0
  | otherwise = case sort [squaredDistance a b, squaredDistance b c, squaredDistance c a] of
      -- the smallest angle is opposite the shortest side; law of cosines
      [s1, s2, s3] -> acos (max (-1) (min 1 ((s2 + s3 - s1) / (2 * sqrt (s2 * s3))))) * 180 / pi
      _ -> 0

squaredDistance :: Point -> Point -> Double
squaredDistance (Point x1 y1) (Point x2 y2) = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)

-- | The centre of the circumcircle; 'Nothing' for a flat triangle. Computed
-- in floating point, relative to the first vertex.
circumcenter :: Triangle -> Maybe Point
circumcenter (Triangle a@(Point ax ay) b@(Point bx by) c@(Point cx cy))
  | orientation a b c == Collinear = Nothing
  | otherwise = Just (Point (ax + ux) (ay + uy))
  where
    (bx', by') = (bx - ax, by - ay)
    (cx', cy') = (cx - ax, cy - ay)
    d = 2 * (bx' * cy' - by' * cx')
    (b2, c2) = (bx' * bx' + by' * by', cx' * cx' + cy' * cy')
    ux = (cy' * b2 - by' * c2) / d
    uy = (bx' * c2 - cx' * b2) / d

-- | Whether the point lies outside of, or exactly on, the circumcircle of the
-- triangle: the Delaunay in-circle test. Exact (see
-- "Triangulation.Geometry.Exact"); a degenerate triangle has no circumcircle
-- and every point counts as outside.
isOutsideCircumcircle :: Point -> Triangle -> Bool
isOutsideCircumcircle p (Triangle a b c) = case orientation a b c of
  CounterClockwise -> inCircle /= GT
  Clockwise -> inCircle /= LT
  Collinear -> True
  where
    inCircle = inCircleSign (coordinates a) (coordinates b) (coordinates c) (coordinates p)

-- | Whether the point lies inside the triangle, in the symbolically perturbed
-- sense of 'turn': a point exactly on an edge is consistently assigned to one
-- side of it. Vertices are excluded.
isPointInTriangle :: Triangle -> Point -> Bool
isPointInTriangle (Triangle a b c) p =
  p `notElem` [a, b, c] && not (Clockwise `elem` turns && CounterClockwise `elem` turns)
  where
    turns = [turn a b p, turn b c p, turn c a p]

-- | A candidate triangle @p1 p2 p3@ is accepted when the walk @p1 -> p2 -> p3@
-- turns clockwise in the perturbed sense of 'turn' (the merge front advances
-- on that side) and no other point lies inside it. A collinear triple passes
-- as a zero-area triangle of the perturbed points; "Triangulation.Repair"
-- removes those afterwards.
isValidCandidate :: Point -> Point -> Point -> [Point] -> Bool
isValidCandidate p1 p2 p3 points =
  turn p1 p2 p3 == Clockwise && not (any (isPointInTriangle (mkTriangle p1 p2 p3)) points)

-- | Whether the centroid of the triangle lies inside the polygon.
isTriangleInPolygon :: Triangle -> Polygon -> Bool
isTriangleInPolygon (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) polygon =
  isPointInPolygon polygon (Point ((x1 + x2 + x3) / 3) ((y1 + y2 + y3) / 3))

-- | The triangles whose centroid lies inside the polygon.
trianglesInside :: Polygon -> [Triangle] -> [Triangle]
trianglesInside polygon = filter (`isTriangleInPolygon` polygon)

-- | The triangles whose centroid lies outside the polygon.
trianglesOutside :: Polygon -> [Triangle] -> [Triangle]
trianglesOutside polygon = filter (not . (`isTriangleInPolygon` polygon))
