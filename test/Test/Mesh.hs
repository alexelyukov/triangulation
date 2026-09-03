module Test.Mesh (tests) where

import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector qualified as V
import Test.Geometry (Points (..))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Property, QuickCheckTests (..), counterexample, testProperty)
import Triangulation

tests :: TestTree
tests =
  localOption (QuickCheckTests 300) $
    testGroup
      "Mesh"
      [ testProperty "nodes are the distinct vertices in ascending order" $
          meshHolds $ \ts (Mesh ns _) ->
            V.toList ns == sort (nub (concatMap (\(Triangle a b c) -> [a, b, c]) ts))
      , testProperty "elements index the nodes, ascending, no repeats" $
          meshHolds $ \_ (Mesh ns es) ->
            let n = V.length ns
                triples = V.toList es
             in all (\(i, j, k) -> all (\x -> 0 <= x && x < n) [i, j, k]) triples
                  && and (zipWith (<) triples (drop 1 triples))
      , testProperty "every element is wound counter-clockwise" $
          meshHolds $ \_ m@(Mesh ns _) ->
            all
              (\(i, j, k) -> orientation (ns V.! i) (ns V.! j) (ns V.! k) == CounterClockwise)
              (V.toList (elements m))
      , testProperty "fromMesh . toMesh is the identity on the set of triangles" $
          meshHolds $
            \ts m -> sort (fromMesh m) == sort (nub ts)
      , testProperty "the boundary of a Delaunay triangulation is its hull" $
          \(Points points) -> case triangulate points of
            Nothing -> counterexample "triangulate returned Nothing" False
            Just t ->
              let m = toMesh (triangles t)
               in counterexample (show m) $
                    edgesOf m (boundaryEdges m) == sort (polygonEdges (hull t))
      , testProperty "the boundary of a polygon with a hole is both polygons" $
          \(Points points) -> case constrainedTriangulate canvas [hole] points of
            Nothing -> counterexample "constrainedTriangulate returned Nothing" False
            Just ts ->
              let m = toMesh ts
               in counterexample (show m) $
                    edgesOf m (boundaryEdges m) == sort (polygonEdges canvas ++ polygonEdges hole)
      , testCase "a single triangle" $
          toMesh [mkTriangle (Point 0 0) (Point 1 0) (Point 0 1)]
            @?= Mesh (V.fromList [Point 0 0, Point 0 1, Point 1 0]) (V.fromList [(0, 2, 1)])
      , testCase "no triangles" $
          toMesh [] @?= Mesh V.empty V.empty
      ]
  where
    canvas = Polygon (Point 0 0 :| [Point 0 2000, Point 2000 2000, Point 2000 0])
    hole = Polygon (Point 600 500 :| [Point 600 1300, Point 1400 1300, Point 1400 500])
    edgesOf (Mesh ns _) = sort . map (\(i, j) -> mkEdge (ns V.! i) (ns V.! j))

-- | The property must hold for the mesh of every Delaunay triangulation of a
-- random point set.
meshHolds :: ([Triangle] -> Mesh -> Bool) -> Points -> Property
meshHolds p (Points points) = case triangulate points of
  Nothing -> counterexample "triangulate returned Nothing" False
  Just t ->
    let ts = triangles t
        m = toMesh ts
     in counterexample (show m) (p ts m)
