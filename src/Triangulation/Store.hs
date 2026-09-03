{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | The triangles of a triangulation, indexed by edge: every edge maps to the
-- (at most two) triangles it belongs to.
--
-- Meant to be imported qualified:
--
-- > import Triangulation.Store (Store)
-- > import Triangulation.Store qualified as Store
module Triangulation.Store (
  Store (..),
  empty,
  null,
  anyTriangle,
  union,
  member,
  hasTriangle,
  trianglesOn,
  edges,
  triangles,
  insert,
  delete,
) where

import Control.DeepSeq (NFData)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as List
import GHC.Generics (Generic)
import Triangulation.Geometry.Edge (Edge)
import Triangulation.Geometry.Triangle (Triangle, triangleEdges)
import Prelude hiding (null)

-- | Triangles indexed by their edges.
-- | The triangles on one edge are kept as a short list rather than a
-- 'Data.HashSet.HashSet': an edge belongs to one or two triangles, and a hash
-- set of that size costs an allocation per insertion for nothing.
newtype Store = Store (HM.HashMap Edge Adjacent)
  deriving stock (Show)
  deriving newtype (NFData)

-- | The triangles on one edge. In a planar triangulation an edge belongs to
-- one or two of them, and spelling that out keeps the common case free of
-- cons cells and of a set per edge. 'Several' exists only so that the type is
-- total: nothing in the library builds it.
data Adjacent
  = One !Triangle
  | Two !Triangle !Triangle
  | Several ![Triangle]
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

adjacentList :: Adjacent -> [Triangle]
adjacentList (One t) = [t]
adjacentList (Two t u) = [t, u]
adjacentList (Several ts) = ts
{-# INLINE adjacentList #-}

-- | No triangles.
empty :: Store
empty = Store HM.empty

-- | Whether the store holds no triangles.
null :: Store -> Bool
null (Store store) = HM.null store

-- | Whether some triangle satisfies the predicate. Cheaper than filtering
-- 'triangles': no set of all triangles is built.
anyTriangle :: (Triangle -> Bool) -> Store -> Bool
anyTriangle p (Store store) = any (any p . adjacentList) (HM.elems store)

-- | The triangles of both stores; meant for stores on disjoint point sets.
union :: Store -> Store -> Store
union (Store store1) (Store store2) = Store $ HM.union store1 store2

-- | Whether the edge belongs to some triangle of the store.
member :: Edge -> Store -> Bool
member edge (Store store) = HM.member edge store

-- | Whether the triangle is in the store.
hasTriangle :: Triangle -> Store -> Bool
hasTriangle triangle (Store store) = case triangleEdges triangle of
  edge : _ -> any (elem triangle . adjacentList) (HM.lookup edge store)
  [] -> False

-- | The triangles the edge belongs to.
trianglesOn :: Edge -> Store -> [Triangle]
trianglesOn edge (Store store) = maybe [] adjacentList (HM.lookup edge store)

-- | Every edge of every triangle.
edges :: Store -> [Edge]
edges (Store store) = HM.keys store

-- | Every triangle, once.
triangles :: Store -> [Triangle]
triangles (Store store) = HS.toList . HS.fromList . concatMap adjacentList $ HM.elems store

-- | Add a triangle under each of its edges.
insert :: Triangle -> Store -> Store
insert triangle (Store store) = Store $ List.foldl' insertOn store (triangleEdges triangle)
  where
    insertOn store' edge = HM.insertWith addUnique edge (One triangle) store'
    addUnique _ present = case present of
      One t | t /= triangle -> Two t triangle
      Two t u | t /= triangle && u /= triangle -> Several [triangle, t, u]
      Several ts | triangle `notElem` ts -> Several (triangle : ts)
      _ -> present

-- | Remove a triangle from each of its edges; edges left without triangles disappear.
delete :: Triangle -> Store -> Store
delete triangle (Store store) = Store $ List.foldl' deleteOn store (triangleEdges triangle)
  where
    deleteOn store' edge = HM.update remaining edge store'
    remaining present = case present of
      One t | t == triangle -> Nothing
      Two t u | t == triangle -> Just (One u)
      Two t u | u == triangle -> Just (One t)
      Several ts -> case filter (/= triangle) ts of
        [] -> Nothing
        [t] -> Just (One t)
        [t, u] -> Just (Two t u)
        rest -> Just (Several rest)
      _ -> Just present
