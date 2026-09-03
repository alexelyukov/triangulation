-- | Constrained Delaunay triangulation and mesh refinement in 2D.
--
-- * 'triangulate' builds the Delaunay triangulation of a point set by
--   divide and conquer.
-- * 'constrainedTriangulate' triangulates a polygon with holes.
-- * 'refine' inserts vertices until every triangle meets an angle and area
--   bound (Ruppert's algorithm), keeping the boundary and hole edges.
-- * 'toMesh' numbers the vertices and gives the triangles as index triples,
--   the form finite element code and mesh file formats expect.
--
-- Coordinates follow the mathematical convention (y axis up); polygons are
-- wound clockwise.
module Triangulation (
  Triangulation (..),
  triangles,
  triangulate,
  constrainedTriangulate,
  Quality (..),
  defaultQuality,
  refine,
  refineWithBudget,
  Mesh (..),
  toMesh,
  fromMesh,
  boundaryEdges,
  module Triangulation.Geometry,
) where

import Triangulation.Constrained (constrainedTriangulate)
import Triangulation.Geometry
import Triangulation.Mesh (Mesh (..), boundaryEdges, fromMesh, toMesh)
import Triangulation.Parallel (triangulate)
import Triangulation.Refine (Quality (..), defaultQuality, refine, refineWithBudget)
import Triangulation.Types (Triangulation (..), triangles)
