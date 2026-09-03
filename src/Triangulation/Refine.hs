{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Delaunay refinement: Ruppert's algorithm.
--
-- Vertices are inserted into a constrained Delaunay triangulation until every
-- triangle meets the quality bounds. The /segments/ — the edges that belong to
-- a single triangle, that is the boundary of the region and of its holes —
-- are preserved as chains: a segment is only ever split at a point on it.
--
-- The two rules, applied until neither fires:
--
-- 1. A segment /encroached/ upon by a vertex (one strictly inside its
--    diametral circle) is split.
-- 2. A poor triangle (an angle below the bound, or an area above it) has its
--    circumcenter inserted — unless the circumcenter would encroach upon
--    segments, in which case those segments are split instead.
--
-- Every insertion is a Bowyer–Watson step: the triangles whose circumcircles
-- contain the new vertex are removed, without crossing a segment, and the
-- cavity is re-triangulated as a fan around the vertex, so the triangulation
-- stays constrained Delaunay.
--
-- Segments meeting at an input vertex at an angle below 60° are handled
-- the way Shewchuk's Triangle does: a segment with one such endpoint is
-- split at a power-of-two distance from it (concentric shells), and a
-- triangle whose smallest angle is formed by two segments is not asked to
-- meet the angle bound, since no refinement can fix an input angle.
module Triangulation.Refine (
  Quality (..),
  defaultQuality,
  refine,
  refineWithBudget,
) where

import Control.DeepSeq (NFData)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sortOn)
import GHC.Generics (Generic)
import Triangulation.Geometry.Edge (Edge (..), mkEdge)
import Triangulation.Geometry.Point (Orientation (..), Point (..), orientation)
import Triangulation.Geometry.Triangle (
  Triangle (..),
  circumcenter,
  isOutsideCircumcircle,
  mkTriangle,
  smallestAngle,
  triangleArea,
  triangleEdges,
 )
import Triangulation.Store (Store)
import Triangulation.Store qualified as Store

-- | What the refined triangulation must satisfy.
data Quality = Quality
  { minAngle :: !Double
  -- ^ lower bound on every angle, in degrees; @0@ imposes none. Ruppert's
  -- algorithm is guaranteed to terminate up to about 20.7°, and usually does
  -- up to about 30°.
  , maxArea :: !(Maybe Double)
  -- ^ upper bound on the area of a triangle
  , maxInsertions :: !Int
  -- ^ how many vertices may be inserted before giving up
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A 20° angle bound, no area bound, at most 100 000 insertions.
defaultQuality :: Quality
defaultQuality = Quality {minAngle = 20, maxArea = Nothing, maxInsertions = 100000}

-- | Refine the triangles of a region — typically the output of
-- 'Triangulation.Constrained.constrainedTriangulate' — until every triangle
-- meets the bounds. 'Nothing' when the insertion budget runs out, which is
-- what happens when the bounds cannot be met.
refine :: Quality -> [Triangle] -> Maybe [Triangle]
refine quality triangles = case refineWithBudget quality triangles of
  (True, refined) -> Just refined
  (False, _) -> Nothing

-- | Like 'refine', but never gives up empty-handed. The 'Bool' says whether
-- the bounds were met; when the budget ran out it is 'False' and the
-- triangles are the mesh reached so far — a valid constrained Delaunay
-- triangulation of the region that does not meet the bounds.
refineWithBudget :: Quality -> [Triangle] -> (Bool, [Triangle])
refineWithBudget quality triangles
  | null triangles = (True, [])
  | otherwise = let (done, r) = loop quality initial in (done, Store.triangles (store r))
  where
    store0 = foldr Store.insert Store.empty triangles
    segments0 = HS.fromList [e | e <- Store.edges store0, [_] <- [Store.trianglesOn e store0]]
    initial =
      Refinement
        { store = store0
        , count = length triangles
        , segments = segments0
        , acute = acuteVertices segments0
        , budget = maxInsertions quality
        , pendingSegments = HS.toList segments0
        , pendingTriangles = triangles
        }

-- | The state of the refinement.
data Refinement = Refinement
  { store :: !Store
  , count :: !Int
  -- ^ number of triangles in the store
  , segments :: !(HashSet Edge)
  , acute :: !(HashSet Point)
  -- ^ input vertices where two segments meet at less than 60°
  , budget :: !Int
  , pendingSegments :: ![Edge]
  -- ^ segments to test for encroachment
  , pendingTriangles :: ![Triangle]
  -- ^ triangles to test for quality
  }

-- | Split encroached segments first; when none is left, fix poor triangles.
-- 'False' when the budget ran out, with the state at that point.
loop :: Quality -> Refinement -> (Bool, Refinement)
loop quality r = case pendingSegments r of
  s : rest
    | isSegment s r && encroachedByApex s r -> continue (splitSegment s r {pendingSegments = rest})
    | otherwise -> loop quality r {pendingSegments = rest}
  [] -> case pendingTriangles r of
    t : rest
      | Store.hasTriangle t (store r) && isPoor quality r t ->
          continue (fixTriangle t r {pendingTriangles = rest})
      | otherwise -> loop quality r {pendingTriangles = rest}
    [] -> (True, r)
  where
    continue = maybe (False, r) (loop quality)

isSegment :: Edge -> Refinement -> Bool
isSegment e r = HS.member e (segments r)

-- | Whether the point lies strictly inside the diametral circle of the edge.
encroaches :: Point -> Edge -> Bool
encroaches (Point x y) (Edge (Point x1 y1) (Point x2 y2)) = (x - x1) * (x - x2) + (y - y1) * (y - y2) < 0

-- | A segment encroached upon by any vertex is encroached upon by the apex of
-- a triangle on it (Shewchuk), so only the apexes need testing.
encroachedByApex :: Edge -> Refinement -> Bool
encroachedByApex e r = any (`encroaches` e) [apex e t | t <- Store.trianglesOn e (store r)]

-- | The vertex of the triangle not on the edge.
apex :: Edge -> Triangle -> Point
apex (Edge a b) (Triangle x y z) = case filter (`notElem` [a, b]) [x, y, z] of
  p : _ -> p
  [] -> x -- unreachable: an edge of a triangle has exactly two of its vertices

-- | Input vertices where two segments meet at an angle below 60°.
acuteVertices :: HashSet Edge -> HashSet Point
acuteVertices segs = HS.fromList [v | (v, ns) <- HM.toList neighbours, hasSmallAngle v ns]
  where
    neighbours = HM.fromListWith (++) (concat [[(a, [b]), (b, [a])] | Edge a b <- HS.toList segs])
    hasSmallAngle v ns = or [cosine v n1 n2 > 0.5 | (n1, i) <- zip ns [0 :: Int ..], (n2, j) <- zip ns [0 ..], i < j]
    cosine (Point x0 y0) (Point x1 y1) (Point x2 y2) =
      let (dx1, dy1, dx2, dy2) = (x1 - x0, y1 - y0, x2 - x0, y2 - y0)
       in (dx1 * dx2 + dy1 * dy2) / sqrt ((dx1 * dx1 + dy1 * dy1) * (dx2 * dx2 + dy2 * dy2))

-- | Whether the triangle violates a bound. The angle bound is waived when the
-- smallest angle lies between two segments: it is an input angle.
isPoor :: Quality -> Refinement -> Triangle -> Bool
isPoor quality r t = tooSmallAngle || tooLarge
  where
    tooSmallAngle = minAngle quality > 0 && smallestAngle t < minAngle quality && not inputAngle
    tooLarge = any (triangleArea t >) (maxArea quality)
    inputAngle = all (`isSegment` r) (longerEdges t)
    -- the smallest angle is opposite the shortest edge, between the two others
    longerEdges = drop 1 . sortOn edgeLength . triangleEdges
    edgeLength (Edge (Point x1 y1) (Point x2 y2)) = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)

-- | Split a segment at its midpoint, or at a power-of-two distance from an
-- acute endpoint, and insert the new vertex.
splitSegment :: Edge -> Refinement -> Maybe Refinement
splitSegment e@(Edge a b) r
  | p == a || p == b = Just r -- too short to split in floating point
  | otherwise = case Store.trianglesOn e (store r) of
      t : _ -> insertOnSegment p e t r
      [] -> Just r
  where
    p
      | HS.member b (acute r) = shell b a
      | HS.member a (acute r) = shell a b
      | otherwise = Point ((px a + px b) / 2) ((py a + py b) / 2)
    -- the split point at a power-of-two distance from @from@, between a third
    -- and two thirds of the way, so that splits on different segments meeting
    -- at the acute vertex land on common concentric circles
    shell from to =
      let len = sqrt ((px to - px from) ^ (2 :: Int) + (py to - py from) ^ (2 :: Int))
          d = 2 ^^ (floor (logBase 2 (2 * len / 3)) :: Int)
          f = d / len
       in Point (px from + f * (px to - px from)) (py from + f * (py to - py from))

-- | Insert the circumcenter of a poor triangle, or split the segments it
-- would encroach upon.
fixTriangle :: Triangle -> Refinement -> Maybe Refinement
fixTriangle t r = case circumcenter t of
  Nothing -> Just r
  Just c -> case locate c t r of
    -- The circumcenter lies outside the region: as in Shewchuk's Triangle,
    -- the segment the walk left through is split instead.
    Exited e -> splitEncroached c [e]
    Lost -> Just r
    Found tc
      | isVertex c tc -> Just r
      | otherwise ->
          let cav = cavity c tc r
              boundary = cavityBoundary cav
           in case [e | e <- boundary, isSegment e r, encroaches c e] of
                [] -> insertInCavity c [] cav boundary r
                encroached -> splitEncroached c encroached
  where
    -- Split the segments and look at the triangle again afterwards; if
    -- nothing could be split (the segments are too short to halve), leave it.
    splitEncroached _ encroached = do
      r' <- foldr (\e acc -> acc >>= splitSegment e) (Just r) encroached
      pure $ if budget r' < budget r then requeue r' else r'
    requeue r' = r' {pendingTriangles = pendingTriangles r' ++ [t]}
    isVertex p (Triangle x y z) = p `elem` [x, y, z]

-- | Where a walk from a triangle towards a point ends.
data Location
  = -- | the triangle containing the point (possibly on its boundary)
    Found Triangle
  | -- | the walk left the region through this segment
    Exited Edge
  | -- | the walk did not terminate in a reasonable number of steps
    Lost

-- | Walk from the triangle towards the point, crossing at each step an edge
-- that separates the point from the current triangle.
locate :: Point -> Triangle -> Refinement -> Location
locate p start r = go (0 :: Int) Nothing start
  where
    go steps cameFrom t
      | steps > count r = Lost
      | inside t = Found t
      | otherwise = case [e | (e@(Edge u v), w) <- edgesWithApex t, Just e /= cameFrom, separates u v w] of
          [] -> Lost
          e : _
            | isSegment e r -> Exited e
            | otherwise -> case filter (/= t) (Store.trianglesOn e (store r)) of
                n : _ -> go (steps + 1) (Just e) n
                [] -> Exited e
    inside (Triangle a b c) = all sameSide [(a, b, c), (b, c, a), (c, a, b)]
    sameSide (u, v, w) = let o = orientation u v p in o == Collinear || o == orientation u v w
    separates u v w = let o = orientation u v p in o /= Collinear && o /= orientation u v w
    edgesWithApex (Triangle a b c) = [(mkEdge a b, c), (mkEdge b c, a), (mkEdge c a, b)]

-- | The triangles whose circumcircles contain the point, reached from the
-- containing triangle without crossing a segment.
cavity :: Point -> Triangle -> Refinement -> HashSet Triangle
cavity p start r = go (HS.singleton start) [start]
  where
    go seen [] = seen
    go seen (t : queue) =
      let fresh =
            [ n
            | e <- triangleEdges t
            , not (isSegment e r)
            , n <- Store.trianglesOn e (store r)
            , n /= t
            , not (HS.member n seen)
            , not (isOutsideCircumcircle p n)
            ]
       in go (foldr HS.insert seen fresh) (fresh ++ queue)

-- | The edges of the cavity that belong to exactly one of its triangles.
cavityBoundary :: HashSet Triangle -> [Edge]
cavityBoundary cav =
  [ e
  | (e, 1 :: Int) <- HM.toList (HM.fromListWith (+) [(e, 1) | t <- HS.toList cav, e <- triangleEdges t])
  ]

-- | Insert a vertex on the given segment, which has the given triangle on it.
-- The segment is split into two at the vertex; it is not tested for
-- collinearity, because the split point is computed in floating point and
-- need not lie exactly on the line.
insertOnSegment :: Point -> Edge -> Triangle -> Refinement -> Maybe Refinement
insertOnSegment p e t r =
  let cav = cavity p t r
   in insertInCavity p [e] cav (cavityBoundary cav) r

-- | Replace the cavity by a fan of triangles around the new vertex. The given
-- edges, and any boundary edge exactly collinear with the vertex, are the
-- segments the vertex lies on: each is split into two segments instead of
-- becoming a flat triangle.
insertInCavity :: Point -> [Edge] -> HashSet Triangle -> [Edge] -> Refinement -> Maybe Refinement
insertInCavity p splitting cav boundary r
  | budget r <= 0 = Nothing
  | otherwise =
      Just
        r
          { store = foldr Store.insert (foldr Store.delete (store r) (HS.toList cav)) newTriangles
          , count = count r - HS.size cav + length newTriangles
          , segments = foldr HS.insert (foldr HS.delete (segments r) split) newSegments
          , budget = budget r - 1
          , pendingSegments = newSegments ++ filter (`isSegment` r) fan ++ pendingSegments r
          , pendingTriangles = newTriangles ++ pendingTriangles r
          }
  where
    isSplit e@(Edge a b) = e `elem` splitting || orientation a b p == Collinear
    split = filter isSplit boundary
    fan = filter (not . isSplit) boundary
    newTriangles = [mkTriangle a b p | Edge a b <- fan]
    newSegments = concat [[mkEdge a p, mkEdge p b] | e@(Edge a b) <- split, isSegment e r]
