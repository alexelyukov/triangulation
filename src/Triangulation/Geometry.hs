-- | Geometric primitives of the library, re-exported from one place.
module Triangulation.Geometry (
  module Triangulation.Geometry.Point,
  module Triangulation.Geometry.Edge,
  module Triangulation.Geometry.Triangle,
  module Triangulation.Geometry.Polygon,
  module Triangulation.Geometry.Ring,
  module Triangulation.Geometry.Exact,
) where

import Triangulation.Geometry.Edge
import Triangulation.Geometry.Exact
import Triangulation.Geometry.Point
import Triangulation.Geometry.Polygon
import Triangulation.Geometry.Ring
import Triangulation.Geometry.Triangle
