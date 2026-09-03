# pure-cdt

[![CI](https://github.com/alexelyukov/triangulation/actions/workflows/ci.yml/badge.svg)](https://github.com/alexelyukov/triangulation/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/pure-cdt.svg)](https://hackage.haskell.org/package/pure-cdt)

Constrained Delaunay triangulation and mesh refinement in 2D, written in Haskell with
no bindings to a C or C++ library.

![a gear with a keyed bore, triangulated and refined to a 28° minimum angle](https://raw.githubusercontent.com/alexelyukov/triangulation/master/assets/gear.png)

*A gear with a keyed bore: `constrainedTriangulate` on the two outlines alone, then
`refine` to a 28° minimum angle and a maximum triangle area. The only input points are
the polygon vertices.*

## Usage

```haskell
import Data.List.NonEmpty (NonEmpty (..))
import Triangulation

-- Delaunay triangulation of a point set (Nothing if it spans no triangle).
Just t = triangulate [Point 0 0, Point 4 0, Point 2 3, Point 2 1]
triangles t  -- :: [Triangle]
hull t       -- :: Polygon, the convex hull

-- A polygon with holes: the boundary and hole edges are forced into the
-- triangulation, triangles outside the boundary or inside a hole are dropped.
-- The third argument holds interior points, if there are any.
boundary = Polygon (Point 0 0 :| [Point 0 10, Point 10 10, Point 10 0])
hole     = Polygon (Point 4 4 :| [Point 4 6, Point 6 6, Point 6 4])
Just ts = constrainedTriangulate boundary [hole] []

-- Mesh refinement (Ruppert's algorithm): insert vertices until no angle is
-- below 25° and no triangle is larger than 0.5, keeping the polygon edges.
Just fine = refine defaultQuality {minAngle = 25, maxArea = Just 0.5} ts

-- The indexed form finite element code wants: nodes, and triangles as
-- counter-clockwise triples of node indices; plus the boundary as index pairs.
mesh = toMesh fine
nodes mesh         -- :: Vector Point
elements mesh      -- :: Vector (Int, Int, Int)
boundaryEdges mesh -- :: [(Int, Int)], the edges that belong to a single triangle
```

Two conventions to know before the first call: the y axis points up, and polygons are
wound clockwise, so that walking along a boundary the interior is on the right. `Edge`
and `Triangle` are abstract; build them with `mkEdge`/`mkTriangle` and take them apart
with the read-only patterns `Edge a b`/`Triangle a b c`.

## What this package is for

It does not compete on speed. A mature Delaunay implementation in C or C++ is an order
of magnitude or two faster, and nothing here will close that gap. What it offers
instead:

* **A pure API.** A handful of ordinary functions that can be mapped over a list or
  used inside a QuickCheck property. A point set that spans no triangle comes back as
  `Nothing`: there are no partial functions and no exceptions in the library.
* **No toolchain beyond GHC.** The dependencies are `base`, `deepseq`, `hashable`,
  `parallel`, `random`, `unordered-containers` and `vector`, so the package builds
  wherever GHC does, including under Nix, when cross-compiling, and on the JavaScript
  and WebAssembly backends.
* **Degenerate input handled, not merely tolerated.** The predicates are exact and the
  algorithm runs on symbolically perturbed points (Edelsbrunner and Mücke's Simulation
  of Simplicity), so lattices, regular polygons and concentric shapes are triangulated
  correctly instead of coming out with holes or overlaps. They cost about three times
  as long as points in general position, and that is a constant factor, not a worse
  complexity.
* **A small readable core.** The code reads as geometry rather than as bookkeeping of
  array indices.

Making it faster would mean giving up the last of those. Passing indices into an array
instead of `Point` values through the algorithm is likely worth a factor of two or
three, and that trade has been declined deliberately.

## Performance

Wall-clock time, median of three runs on an Intel Core i7-14700KF (eight performance
cores plus twelve efficiency cores), GHC 9.10.3:

| Points | Triangles | 1 core | 4 cores | 8 cores |
| ---: | ---: | ---: | ---: | ---: |
| 50 000 | 99 967 | 0.41 s | 0.17 s (2.4×) | 0.13 s (3.2×) |
| 100 000 | 199 965 | 0.86 s | 0.37 s (2.3×) | 0.30 s (2.9×) |
| 500 000 | 999 966 | 5.11 s | 2.57 s (2.0×) | 2.15 s (2.4×) |
| 1 000 000 | 1 999 968 | 11.4 s | 5.80 s (2.0×) | 5.16 s (2.2×) |

Halves of a large point set are triangulated in parallel and then merged. The speedup
plateaus between eight and twelve threads and falls away after that, because the merges
along the spine of the recursion are sequential and the parallel collector has more
capabilities to synchronise.

## Building

```
stack build          # library and the example executable
stack test           # tasty: unit tests and QuickCheck properties
stack bench          # tasty-bench: triangulate, refine, toMesh
stack exec pure-cdt-examples   # renders the examples to assets/*.png

# wall-clock scaling, and the same on a regular lattice
stack bench pure-cdt:pure-cdt-scaling --ba '50000 500000 +RTS -N8'
stack bench pure-cdt:pure-cdt-scaling --ba 'lattice 100000 +RTS -N8'
```

`tasty-bench` reports CPU time, which grows with the number of cores and so says
nothing about a parallel speedup; the table above comes from `pure-cdt-scaling`, which
measures the clock. The example renderer sits behind the cabal flag `examples`, off by
default, so that the library carries no image dependencies; `stack.yaml` turns it on
for development, and plain cabal takes `-f examples`.

Formatting is `fourmolu`, linting is `hlint`; both run in CI.

## Limitations

* Polygons must be simple and must not cross each other. A hole may touch the boundary
  at a vertex, in which case the corner is cut off.
* Input whose points all lie on one line has no triangulation, and `triangulate`
  returns `Nothing` for it.
* Ruppert's refinement is guaranteed to terminate for angle bounds up to about 20.7°
  when no two polygon edges meet at less than 60°, and in practice works up to about
  30°. A demanding bound next to a small input angle can exhaust the insertion budget;
  `refine` then returns `Nothing`, and `refineWithBudget` returns the mesh it reached.
* `refine` works on the triangles of a region, so an interior constraint edge with
  triangles on both sides is not preserved. It splits segments at points computed in
  floating point, so a vertex it adds to the boundary lies on the original polygon edge
  only up to rounding.

## License

BSD-3-Clause.
