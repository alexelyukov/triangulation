{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | The indexed form of a triangulation: an array of nodes and the triangles
-- as triples of node indices. This is the shape finite element assembly,
-- mesh file formats and graphics APIs expect, and it is where a triangulation
-- built from 'Point's by coordinate turns into
-- one addressed by integers.
--
-- The output is canonical: nodes are numbered in ascending 'Ord' order of the
-- points, triangles are listed in ascending order of their index triples, and
-- every triple is wound counter-clockwise (with the y axis up, so its signed
-- area is positive). Two runs on the same triangles give the same mesh.
module Triangulation.Mesh (
  Mesh (..),
  toMesh,
  fromMesh,
  boundaryEdges,
) where

import Control.DeepSeq (NFData)
import Data.HashMap.Strict qualified as HM
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Triangulation.Geometry.Point (Orientation (..), Point, orientation)
import Triangulation.Geometry.Triangle (Triangle (..), mkTriangle)

-- | A triangulation with its vertices numbered.
data Mesh = Mesh
  { nodes :: !(Vector Point)
  -- ^ the distinct vertices, in ascending order
  , elements :: !(Vector (Int, Int, Int))
  -- ^ the triangles as indices into 'nodes', each wound counter-clockwise
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Number the vertices of the triangles and express the triangles through
-- the numbers. Duplicate triangles are kept once. A zero-area triangle,
-- which has no winding, keeps its vertices in ascending order.
toMesh :: [Triangle] -> Mesh
toMesh triangles = Mesh (V.fromList points) (V.fromList indexed)
  where
    points = distinct $ concatMap (\(Triangle a b c) -> [a, b, c]) triangles
    index = HM.fromList (zip points [0 ..])
    -- every vertex is indexed, so the lookups never fail
    indexed = distinct $ mapMaybe counterClockwise triangles
    counterClockwise (Triangle a b c) = case orientation a b c of
      Clockwise -> (,,) <$> at a <*> at c <*> at b
      _ -> (,,) <$> at a <*> at b <*> at c
    at p = HM.lookup p index

-- | Sorted, without repetitions.
distinct :: Ord a => [a] -> [a]
distinct xs = [x | x NE.:| _ <- NE.group (sort xs)]

-- | The triangles back as geometry.
fromMesh :: Mesh -> [Triangle]
fromMesh (Mesh points triangles) =
  [mkTriangle (points V.! i) (points V.! j) (points V.! k) | (i, j, k) <- V.toList triangles]

-- | The edges that belong to exactly one triangle: the outer boundary and the
-- boundaries of the holes. Each edge is a pair of node indices with the
-- smaller one first; the list is in ascending order.
boundaryEdges :: Mesh -> [(Int, Int)]
boundaryEdges (Mesh _ triangles) =
  [e | e NE.:| [] <- NE.group . sort $ concatMap edgesOf (V.toList triangles)]
  where
    edgesOf (i, j, k) = [ordered i j, ordered j k, ordered k i]
    ordered a b = (min a b, max a b)
