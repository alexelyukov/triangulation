# Changelog for `pure-cdt`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.2.0.0 - 2026-09-04

First public release. Version 0.1.0.0 existed only in the repository, under the name
`triangulation`, and was never uploaded.

- `triangulate`: Delaunay triangulation of a point set by divide and conquer, with
  halves of at least 256 points evaluated in parallel through
  `Control.Parallel.Strategies`.
- `constrainedTriangulate`: triangulation of a polygon with holes. The boundary and
  hole edges are forced in, triangles outside the region are dropped, and the boundary
  may be non-convex. An input point on a polygon edge subdivides it.
- `refine`: Delaunay refinement (Ruppert's algorithm) to a minimum angle and a maximum
  triangle area, preserving the boundary and hole edges. `refineWithBudget` returns the
  mesh reached when the insertion budget runs out.
- `toMesh`, `boundaryEdges`: the triangulation as node coordinates and
  counter-clockwise index triples, with the boundary edges identified — the form finite
  element code and mesh file formats expect.
- Exact geometric predicates: orientation and in-circle are decided by a
  floating-point filter backed by `Rational` arithmetic, and the algorithm runs on
  symbolically perturbed points (Simulation of Simplicity), so collinear and
  cocircular input — grids, regular polygons, concentric shapes — is triangulated
  correctly.
- No partial functions and no exceptions in the library: a point set that spans no
  triangle comes back as `Nothing`.
- `Point`, `Edge`, `Triangle` and `Polygon` derive `Eq`, `Ord`, `Hashable` and
  `NFData`. `Edge` and `Triangle` are abstract and normalise their vertices, so
  equality and hashing do not depend on construction order.
- Tests on `tasty`: QuickCheck properties for the triangulation (Delaunay condition,
  planarity, triangle count, convex hull), for the region mesh and for the refinement,
  next to unit tests for each geometric primitive. Benchmarks on `tasty-bench`.
