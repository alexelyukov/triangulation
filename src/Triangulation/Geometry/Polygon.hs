{-# LANGUAGE DerivingStrategies #-}

-- | Simple polygons, convex hulls of three and four points, and the polygon
-- side of merging two triangulations: sliding a bridge between two hulls
-- until it is tangent to both.
--
-- Throughout the library polygons are wound /clockwise/ (with the y axis
-- pointing up): walking along the boundary, the interior is on the right.
module Triangulation.Geometry.Polygon (
  Polygon (..),
  vertices,
  polygonEdges,
  Bridge,
  Hull4 (..),
  hullOf3,
  hullOf4,
  hull4Polygon,
  tangents,
  isConvex,
  isPointInPolygon,
) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Triangulation.Geometry.Edge (Edge, mkEdge)
import Triangulation.Geometry.Point (Orientation (..), Point (..), turn)
import Triangulation.Geometry.Ring (
  arc,
  cyclicPairs,
  cyclicTriples,
  predecessor,
  splitLoop,
  successor,
 )

-- | A simple polygon, wound clockwise; see the module header.
newtype Polygon = Polygon (NonEmpty Point)
  deriving stock (Eq, Show)
  deriving newtype (NFData)

-- | The vertices, in boundary order.
vertices :: Polygon -> [Point]
vertices (Polygon points) = NE.toList points

-- | The boundary edges, including the one closing the ring.
polygonEdges :: Polygon -> [Edge]
polygonEdges (Polygon points) = map (uncurry mkEdge) (cyclicPairs points)

-- hulls of 3 and 4 points

-- | Convex hull of three points, wound clockwise. A collinear triple is kept
-- as a degenerate polygon.
hullOf3 :: Point -> Point -> Point -> Polygon
hullOf3 a b c =
  let (pivot, r1, r2) = sort3By compare a b c
   in Polygon $ case hullOrder pivot r1 r2 of
        GT -> pivot :| [r2, r1]
        _ -> pivot :| [r1, r2]

-- | Convex hull of four points.
data Hull4
  = -- | all four points are hull vertices, wound clockwise
    Quadrilateral Point Point Point Point
  | -- | a hull triangle, wound clockwise, and the point inside it
    TriangleWithInner Point Point Point Point

-- | Convex hull of four points, telling a convex quadrilateral from a
-- triangle with a point inside.
hullOf4 :: Point -> Point -> Point -> Point -> Hull4
hullOf4 a b c d =
  let (p1, r1, r2, r3) = sort4By compare a b c d
      (p2, p3, p4) = sort3By (hullOrder p1) r1 r2 r3
   in case (turn p2 p3 p4, turn p3 p4 p1) of
        (CounterClockwise, _) -> TriangleWithInner p1 p2 p4 p3
        (_, Clockwise) -> Quadrilateral p1 p2 p3 p4
        _ -> TriangleWithInner p1 p2 p3 p4 -- rare case: points on the same line

-- | The hull as a polygon, dropping an inner point.
hull4Polygon :: Hull4 -> Polygon
hull4Polygon (Quadrilateral p1 p2 p3 p4) = Polygon (p1 :| [p2, p3, p4])
hull4Polygon (TriangleWithInner p1 p2 p3 _) = Polygon (p1 :| [p2, p3])

-- | Angular order around a pivot: @a@ precedes @b@ when @b@ lies clockwise of @pivot -> a@.
hullOrder :: Point -> Point -> Point -> Ordering
hullOrder pivot a b = case turn pivot a b of
  Clockwise -> LT
  CounterClockwise -> GT
  Collinear -> EQ -- unreachable: 'turn' resolves collinear triples

sort3By :: (a -> a -> Ordering) -> a -> a -> a -> (a, a, a)
sort3By cmp a b c
  | cmp c lo /= GT = (c, lo, hi)
  | cmp c hi /= GT = (lo, c, hi)
  | otherwise = (lo, hi, c)
  where
    (lo, hi) = if cmp a b == GT then (b, a) else (a, b)

sort4By :: (a -> a -> Ordering) -> a -> a -> a -> a -> (a, a, a, a)
sort4By cmp a b c d
  | cmp a x /= GT = (a, x, y, z)
  | cmp a y /= GT = (x, a, y, z)
  | cmp a z /= GT = (x, y, a, z)
  | otherwise = (x, y, z, a)
  where
    (x, y, z) = sort3By cmp b c d

-- tangents

-- | A pair of vertices joining two polygons being merged:
-- @(vertex of the first polygon, vertex of the second polygon)@.
type Bridge = (Point, Point)

-- | Starting from an initial bridge (used as both the bottom and the top
-- one), slide the bridge endpoints along the two hulls until both bridges are
-- tangent to both polygons. Returns the bottom and top tangents and the merged
-- hull. 'Nothing' if a bridge endpoint is not a vertex of its polygon.
tangents :: Polygon -> Polygon -> Bridge -> Maybe (Bridge, Bridge, Polygon)
tangents (Polygon points1) (Polygon points2) start = go start start
  where
    go bottom@(bl, br) top@(tl, tr) = case slide bottom top of
      Just (bottom', top') -> go bottom' top'
      Nothing -> do
        cut1 <- arc tl bl points1
        cut2 <- arc br tr points2
        pure (bottom, top, removeLoops (Polygon (cut1 <> cut2)) bottom top)

    -- One step: move whichever bridge endpoint still sees its hull neighbour
    -- on the wrong side. 'Nothing' when both bridges are tangent.
    slide (bl, br) (tl, tr) = do
      blPred <- predecessor bl points1
      brSucc <- successor br points2
      tlSucc <- successor tl points1
      trPred <- predecessor tr points2
      pick blPred brSucc tlSucc trPred
      where
        pick blPred brSucc tlSucc trPred
          | turn blPred bl br == CounterClockwise = Just ((blPred, br), (tl, tr))
          | turn bl br brSucc == CounterClockwise = Just ((bl, brSucc), (tl, tr))
          | turn tr tl tlSucc == CounterClockwise = Just ((bl, br), (tlSucc, tr))
          | turn trPred tr tl == CounterClockwise = Just ((bl, br), (tl, trPred))
          | otherwise = Nothing

-- | When a bridge degenerates to a single vertex on one side, the merged ring
-- visits that vertex twice; keep the larger of the two loops.
removeLoops :: Polygon -> Bridge -> Bridge -> Polygon
removeLoops polygon (bl, br) (tl, tr)
  | bl == tl = largerLoop polygon bl
  | br == tr = largerLoop polygon br
  | otherwise = polygon

largerLoop :: Polygon -> Point -> Polygon
largerLoop (Polygon points) point =
  let (outer, inner) = splitLoop point points
   in case NE.nonEmpty inner of
        Just inner' | doubledArea (Polygon outer) <= doubledArea (Polygon inner') -> Polygon inner'
        _ -> Polygon outer

-- | Twice the area (shoelace formula); enough for comparisons.
doubledArea :: Polygon -> Double
doubledArea (Polygon points) =
  abs $ sum [x1 * y2 - y1 * x2 | (Point x1 y1, Point x2 y2) <- cyclicPairs points]

-- checking

-- | Whether the polygon is convex (no counter-clockwise turn along the boundary).
isConvex :: Polygon -> Bool
isConvex (Polygon (_ :| [_])) = True
isConvex (Polygon points) =
  all (\(p1, p2, p3) -> turn p1 p2 p3 /= CounterClockwise) (cyclicTriples points)

-- | Whether the point lies strictly inside the polygon, which need not be
-- convex: the number of polygon edges crossed by a ray from the point to the
-- right is odd (the even–odd rule). A vertex of the polygon does not count as
-- inside; for a point exactly on an edge the answer is not specified.
isPointInPolygon :: Polygon -> Point -> Bool
isPointInPolygon (Polygon points) point@(Point x y) =
  point `notElem` points && odd (length (filter crossesRay (cyclicPairs points)))
  where
    -- half-open in y, so that a ray through a vertex is counted once
    crossesRay (Point x1 y1, Point x2 y2) =
      (y1 > y) /= (y2 > y) && x < x1 + (y - y1) * (x2 - x1) / (y2 - y1)
