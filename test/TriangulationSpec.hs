module TriangulationSpec (
  triangulationOn3PointsTest,
  triangulationOn4PointsTest,
  buildPolygonOn8PointsTest,
  trianglesNotIntersectedTest,
  triangulationDelaunayTest,
  triangulationWithRestrictionsTest
) where

import Test.HUnit ( assertEqual, Test(TestCase) )
import Geometry.Types.Rectangle ( Rectangle(Rectangle, bottomRight, topLeft) )
import Geometry.Types.Point ( Point(..) )
import Geometry.Types.Edge ( Edge(Edge) )
import Geometry.Polygon ( isConvexHull )
import Geometry.Triangle ( rebuildTriangles )
import Generator ( generatePoints )
import Triangulation ( checkDelaunay, checkNotIntersected, Triangulation(..) )
import TriangulationStore ( hasTrianglesOnEdge )
import TriangulationEvaluation ( buildTEvaluation, evalTriangulation )

triangulationOn3PointsTest :: Test
triangulationOn3PointsTest = TestCase (assertEqual "triangulation on 3 points" (all checkDelaunay $ generateTriangulationSet $ generatePointsSet 3 1000) True)

triangulationOn4PointsTest :: Test
triangulationOn4PointsTest = TestCase (assertEqual "triangulation on 4 points" (all checkDelaunay $ generateTriangulationSet $ generatePointsSet 4 1000) True)

buildPolygonOn8PointsTest :: Test
buildPolygonOn8PointsTest = TestCase (assertEqual "build polygon on 8 points" (checkPolygonSet $ generateTriangulationSet $ generatePointsSet 8 1000) True)

trianglesNotIntersectedTest :: Test
trianglesNotIntersectedTest = TestCase (assertEqual "check triangles not intersected" (all checkNotIntersected $ generateTriangulationSet $ generatePointsSet 8 1000) True)

triangulationDelaunayTest :: Test
triangulationDelaunayTest = TestCase (assertEqual "check Delaunay condition on 8 points" (all checkDelaunay $ generateTriangulationSet $ generatePointsSet 8 1000) True >>
                                      assertEqual "check Delaunay condition" (all checkDelaunay $ generateTriangulationSet $ generatePointsSet 1000 1) True)

triangulationWithRestrictionsTest :: Test
triangulationWithRestrictionsTest = TestCase (assertEqual "triangulation with restrictions" (checkTriangulationWithRestrictions 8 1000) True)

generatePointsSet :: Int -> Int -> [[Point]]
generatePointsSet l n = [generatePoints k l Rectangle { topLeft = Point (0, 0), bottomRight = Point (2000, 2000) } | k <- [1..n]]

generateTriangulationSet :: [[Point]] -> [Triangulation]
generateTriangulationSet pointsSets = [(evalTriangulation . buildTEvaluation) points | points <- pointsSets]

checkPolygonSet :: [Triangulation] -> Bool
checkPolygonSet = all (\(Triangulation polygon _) -> isConvexHull polygon)

checkTriangulationWithRestrictions :: Int -> Int -> Bool
checkTriangulationWithRestrictions n m =
  let points = generatePointsSet n m
      pointsForEdges = map (take 2) points
      restrictedEdges = map (\p -> Edge (head p, last p)) pointsForEdges
      triangulations = generateTriangulationSet points
      isDelaunayTriangulationCorrect = all checkDelaunay triangulations
      edgesWithTriangulations = zip restrictedEdges triangulations
      rebuiltEdgesWithTriangulations = map (
        \(edge, Triangulation polygon store) ->
          let edges = [edge]
              filtredEdges = filter (\edge' -> not $ hasTrianglesOnEdge edge' store) edges
          in (edge, Triangulation polygon $ rebuildTriangles store filtredEdges edges)
        ) edgesWithTriangulations
      isEdgesCorrect = all (\(edge, Triangulation _ store) -> hasTrianglesOnEdge edge store) rebuiltEdgesWithTriangulations
      isTrianglesNotCrossed = all (\(_, triangulation) -> checkNotIntersected triangulation) rebuiltEdgesWithTriangulations
  in isDelaunayTriangulationCorrect && isEdgesCorrect && isTrianglesNotCrossed
