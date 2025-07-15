module Main where

import Test.HUnit
import qualified System.Exit as Exit

import TriangulationSpec (
  triangulationOn3PointsTest,
  triangulationOn4PointsTest,
  buildPolygonOn8PointsTest,
  trianglesNotIntersectedTest,
  triangulationDelaunayTest,
  triangulationWithRestrictionsTest
  )

tests :: Test
tests = TestList [
  triangulationOn3PointsTest,
  triangulationOn4PointsTest,
  buildPolygonOn8PointsTest,
  trianglesNotIntersectedTest,
  triangulationDelaunayTest,
  triangulationWithRestrictionsTest
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
