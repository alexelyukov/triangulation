module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf)
import Triangulation (
  Point (..),
  Polygon (..),
  Quality (..),
  constrainedTriangulate,
  defaultQuality,
  refine,
  toMesh,
  triangles,
  triangulate,
 )
import Triangulation.Generator (Rectangle (..), generatePoints)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "triangulate"
        [ bench (show n ++ " points") $ nf (fmap (length . triangles) . triangulate) (points n)
        | n <- [1000, 8000, 50000]
        ]
    , bgroup
        "refine a square with a hole"
        [ bench ("20°, area " ++ show area) $
            nf (fmap length . refine defaultQuality {maxArea = Just area}) coarse
        | area <- [4000, 1000]
        ]
    , bench "toMesh of 8000 points" $ nf (fmap (toMesh . triangles) . triangulate) (points 8000)
    ]
  where
    points n = generatePoints 7 n Rectangle {minCorner = Point 0 0, maxCorner = Point 2000 2000}
    canvas = Polygon (Point 0 0 :| [Point 0 2000, Point 2000 2000, Point 2000 0])
    hole = Polygon (Point 600 500 :| [Point 600 1300, Point 1400 1300, Point 1400 500])
    coarse = fromMaybe [] (constrainedTriangulate canvas [hole] (points 200))
