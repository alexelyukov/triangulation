-- | Repair of degeneracies left behind by collinear input.
--
-- Three collinear points that land in the same leaf of the divide-and-conquer
-- tree become a zero-area triangle, and the merge builds on it: its long edge
-- ends up spanning the middle vertex, which later blocks constrained edges.
-- Once the whole triangulation is assembled these spots are easy to find and
-- fix locally, so that is done here rather than in every step that could
-- produce one.
module Triangulation.Repair (
  repairDegeneracies,
) where

import Data.HashSet qualified as HS
import Data.List (findIndex)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Triangulation.Flip (legalize)
import Triangulation.Geometry.Edge (Edge, mkEdge)
import Triangulation.Geometry.Point (Orientation (..), Point (..), orientation)
import Triangulation.Geometry.Polygon (Polygon (..))
import Triangulation.Geometry.Ring (cyclicPairs)
import Triangulation.Geometry.Triangle (Triangle (..), mkTriangle, triangleEdges)
import Triangulation.Store (Store)
import Triangulation.Store qualified as Store
import Triangulation.Types (Triangulation (..))

-- | Remove every zero-area triangle, re-triangulating its neighbourhood, and
-- put the vertices this leaves on the hull boundary into the hull ring. The
-- given edges are constraints that the Delaunay flips must not remove.
-- Input in general position has no flat triangles and passes through
-- untouched.
repairDegeneracies :: [Edge] -> Triangulation -> Triangulation
repairDegeneracies restrictedEdges triangulation@(Triangulation polygon store)
  | not (Store.anyTriangle isFlat store) = triangulation
  | otherwise =
      let store' = removeFlats restrictedEdges store
          points = HS.toList . HS.fromList $ concatMap (\(Triangle a b c) -> [a, b, c]) (Store.triangles store')
       in Triangulation (foldr insertOnBoundary polygon points) store'

-- | Repair flat triangles until none is left.
--
-- One scan of the store collects every flat triangle there is and all of them
-- are repaired before the store is scanned again, because repairing one can
-- leave a new one behind. Taking the first flat triangle of a fresh scan each
-- time, as this used to, costs a scan per repair; on input like a lattice,
-- where a large share of the triangles is flat, that made the whole
-- triangulation quadratic.
removeFlats :: [Edge] -> Store -> Store
removeFlats restrictedEdges = rounds
  where
    rounds store = case filter isFlat (Store.triangles store) of
      [] -> store
      flats -> rounds (List.foldl' repair store flats)
    repair store flat
      | Store.hasTriangle flat store = repairOne restrictedEdges flat store
      | otherwise = store -- an earlier repair in this round removed it

isFlat :: Triangle -> Bool
isFlat (Triangle a b c) = orientation a b c == Collinear

-- | The vertices of a triangle are kept sorted, so in a flat triangle @a b c@
-- the point @b@ lies between @a@ and @c@ and the edge @a c@ spans it. The
-- triangle on the other side of that edge, @a c d@, is split at @b@ into
-- @a b d@ and @b c d@; without such a triangle the flat one is simply dropped.
repairOne :: [Edge] -> Triangle -> Store -> Store
repairOne restrictedEdges flat@(Triangle a b c) store =
  let store1 = Store.delete flat store
   in case Store.trianglesOn (mkEdge a c) store1 of
        [neighbour@(Triangle x y z)]
          | [d] <- filter (`notElem` [a, c]) [x, y, z] ->
              let replacements = filter (not . isFlat) [mkTriangle a b d, mkTriangle b c d]
                  store2 = foldr Store.insert (Store.delete neighbour store1) replacements
               in legalize store2 (concatMap triangleEdges replacements) restrictedEdges
        _ -> store1

-- | Insert a vertex lying strictly inside a hull edge between that edge's
-- endpoints; a vertex already on the ring, or not on any edge, is left alone.
insertOnBoundary :: Point -> Polygon -> Polygon
insertOnBoundary p polygon@(Polygon ring)
  | p `elem` ring = polygon
  | otherwise = case findIndex (\(u, v) -> liesBetween u v p) (cyclicPairs ring) of
      Just i ->
        let (front, back) = splitAt (i + 1) (NE.toList ring) in Polygon (NE.fromList (front ++ p : back))
      Nothing -> polygon

liesBetween :: Point -> Point -> Point -> Bool
liesBetween u v p =
  p /= u
    && p /= v
    && orientation u v p == Collinear
    && min (px u) (px v) <= px p
    && px p <= max (px u) (px v)
    && min (py u) (py v) <= py p
    && py p <= max (py u) (py v)
