module Main (main) where

import Test.Geometry qualified
import Test.Mesh qualified
import Test.Refine qualified
import Test.Ring qualified
import Test.Tasty (defaultMain, testGroup)
import Test.Triangulation qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "triangulation"
      [ Test.Ring.tests
      , Test.Geometry.tests
      , Test.Triangulation.tests
      , Test.Mesh.tests
      , Test.Refine.tests
      ]
