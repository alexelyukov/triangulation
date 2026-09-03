module Test.Triangulation (tests) where

import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Vector qualified as V
import Test.Geometry (Points (..))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (
  Arbitrary (..),
  Property,
  QuickCheckTests (..),
  chooseInt,
  counterexample,
  property,
  shrinkList,
  testProperty,
  vectorOf,
 )
import Triangulation
import Triangulation.Check (hasNoIntersections, isDelaunay, isLocallyDelaunay)
import Triangulation.Constrained (forceEdges)
import Triangulation.Generator (Rectangle (..), generatePoints)
import Triangulation.Store qualified as Store

tests :: TestTree
tests =
  testGroup
    "Triangulation"
    [ localOption (QuickCheckTests 300) properties
    , localOption (QuickCheckTests 300) collinearHeavy
    , edgeCases
    , largeSets
    , constrained
    , localOption (QuickCheckTests 300) constrainedRegion
    ]

-- | Every triangulation of a random point set must satisfy the predicate.
holds :: String -> (Triangulation -> Bool) -> Points -> Property
holds what p (Points points) = case triangulate points of
  Nothing -> counterexample "triangulate returned Nothing" False
  Just t -> counterexample (what ++ " violated by " ++ show t) (p t)

properties :: TestTree
properties =
  testGroup
    "properties of random point sets"
    [ testProperty "Delaunay condition" $ holds "Delaunay condition" (isDelaunay . triangles)
    , testProperty "no triangle has zero area" $ holds "non-degeneracy" (not . any isFlat . triangles)
    , testProperty "no edge spans another vertex" $ holds "no edge spans a vertex" noEdgeSpansAVertex
    , testProperty "no two edges cross" $ holds "planarity" (hasNoIntersections . triangles)
    , testProperty "Euler's formula: 2n - h - 2 triangles" $ holds "Euler's formula" hasEulerTriangleCount
    , testProperty "the hull is convex" $ holds "convexity" (isConvex . hull)
    , testProperty "the hull contains every point" $
        \ps@(Points points) -> holds "hull containment" (hullContains points) ps
    ]

-- | Points drawn from a coarse grid, so that collinear and cocircular triples
-- are the rule rather than the exception.
collinearHeavy :: TestTree
collinearHeavy =
  testGroup
    "collinear-heavy input (grid points)"
    [ testProperty "Delaunay condition" $ gridHolds (isDelaunay . triangles)
    , testProperty "no two edges cross" $ gridHolds (hasNoIntersections . triangles)
    , testProperty "no triangle has zero area" $ gridHolds (not . any isFlat . triangles)
    , testProperty "no edge spans another vertex" $ gridHolds noEdgeSpansAVertex
    , testProperty "Euler's formula" $ gridHolds hasEulerTriangleCount
    , testProperty "the hull contains every point" $
        \(GridPoints points) -> gridHolds (hullContains points) (GridPoints points)
    ]
  where
    -- all points on one line: no triangulation exists and Nothing is the right answer
    gridHolds p (GridPoints points) = case triangulate points of
      Nothing -> counterexample "triangulate returned Nothing" (allCollinear points)
      Just t -> counterexample (show (triangles t)) (p t)
    allCollinear (a : b : rest) = all (\c -> orientation a b c == Collinear) rest
    allCollinear _ = True

-- | At least three distinct points with coordinates that are multiples of 100;
-- shrinking drops points but never below three.
newtype GridPoints = GridPoints [Point]
  deriving (Show)

instance Arbitrary GridPoints where
  arbitrary = do
    n <- chooseInt (3, 12)
    points <- vectorOf (3 * n) (Point <$> coordinate <*> coordinate)
    pure (GridPoints (take n (nub points)))
    where
      coordinate = (* 100) . fromIntegral <$> chooseInt (0, 20)
  shrink (GridPoints ps) = [GridPoints ps' | ps' <- shrinkList (const []) ps, length ps' >= 3]

isFlat :: Triangle -> Bool
isFlat (Triangle a b c) = orientation a b c == Collinear

-- | No edge of the triangulation has a vertex strictly inside it. Overlapping
-- collinear edges used to slip past 'hasNoIntersections', which only looks for
-- proper crossings.
noEdgeSpansAVertex :: Triangulation -> Bool
noEdgeSpansAVertex triangulation =
  not $ or [spans a b p | Edge a b <- Store.edges (triangleStore triangulation), p <- points]
  where
    points = nub $ concatMap (\(Triangle a b c) -> [a, b, c]) (triangles triangulation)
    spans a b p =
      p /= a
        && p /= b
        && orientation a b p == Collinear
        && min (px a) (px b) <= px p
        && px p <= max (px a) (px b)
        && min (py a) (py b) <= py p
        && py p <= max (py a) (py b)

edgeCases :: TestTree
edgeCases =
  testGroup
    "edge cases"
    [ testCase "fewer than three points give Nothing"
        $ assertBool "expected Nothing"
        $ all (isNothing . triangulate) [[], [Point 1 1], [Point 1 1, Point 2 2]]
    , testCase "five points (split 2 + 3) and 9-11 points (split 3 + rest)" $
        mapM_
          (\n -> assertBool ("size " ++ show n) (all wellFormed (deterministicSets n 200)))
          ([5 .. 12] :: [Int])
    ]

largeSets :: TestTree
largeSets =
  testGroup
    "large deterministic sets"
    [ testCase "1000 points, three seeds"
        $ assertBool "well-formed and Delaunay"
        $ all
          (any (\t -> hasEulerTriangleCount t && isDelaunay (triangles t)) . triangulate)
          (deterministicSets 1000 3)
    , -- A lattice is the worst case for degeneracy and the most likely input of
      -- a finite element model: every row, column and diagonal is collinear and
      -- the corners of every cell are cocircular. Repairing the flat triangles
      -- it produces used to rescan the whole store per repair, which made a
      -- lattice of this size quadratic.
      testCase "a 40x40 lattice" $ case triangulate lattice of
        Nothing -> assertBool "expected a triangulation" False
        Just t -> do
          let ts = triangles t
          assertBool "no flat triangle" (not (any isFlat ts))
          assertBool "locally Delaunay" (isLocallyDelaunay ts)
          assertBool "no edge spans a vertex" (noEdgeSpansAVertex t)
          assertBool "Euler's formula" (hasEulerTriangleCount t)
          -- not 'isConvex': the hull ring of a lattice carries the boundary
          -- points that lie between its corners, and under the symbolic
          -- perturbation those are not collinear, so a strictly convex ring is
          -- the wrong expectation here. The property tests on grid points omit
          -- the same check for the same reason.
          assertBool "the hull contains every point" (hullContains lattice t)
          length (nub (concatMap (\(Triangle a b c) -> [a, b, c]) ts)) @?= length lattice
    ]
  where
    lattice =
      [ Point (fromIntegral i * 300) (fromIntegral j * 300)
      | i <- [0 .. 39 :: Int]
      , j <- [0 .. 39 :: Int]
      ]

constrained :: TestTree
constrained =
  testGroup
    "constrained edges"
    [ testProperty "a forced edge is present and nothing crosses" $
        \(Points points) -> case (points, triangulate points) of
          (a : b : _, Just (Triangulation _ store)) ->
            let edge = mkEdge a b
                store' = forceEdges store [edge | not (Store.member edge store)] [edge]
             in property $ Store.member edge store' && hasNoIntersections (Store.triangles store')
          _ -> counterexample "triangulate returned Nothing" False
    , testProperty "constrainedTriangulate keeps the boundary and empties the hole" $
        \(Points points) -> constrainedHolds hole points
    , testProperty "constrainedTriangulate with the diagonal hole (collinear input)" $
        \(Points points) -> constrainedHolds diagonalHole points
    , -- Regression: hole corners on the canvas diagonals make many triples collinear;
      -- a zero-area candidate triangle used to slip in and swallow a hole edge.
      testCase "hole with corners on the canvas diagonals (collinear input)"
        $ assertBool "boundary and hole edges present, hole empty"
        $ constrainedOk
          diagonalHole
          [ Point 823.1104570397463 1295.569301574011
          , Point 1705.4929691922034 740.4819744884217
          , Point 97.24513811803548 174.33490747208324
          ]
    ]
  where
    canvas = Polygon (Point 0 0 :| [Point 0 2000, Point 2000 2000, Point 2000 0])
    -- corners off the canvas diagonals, so the input is in general position
    hole = Polygon (Point 600 500 :| [Point 600 1300, Point 1400 1300, Point 1400 500])
    diagonalHole = Polygon (Point 500 500 :| [Point 500 1500, Point 1500 1500, Point 1500 500])
    constrainedHolds h points = case constrainedTriangulate canvas [h] points of
      Nothing -> counterexample "constrainedTriangulate returned Nothing" False
      Just ts -> counterexample (show ts) (constrainedOkWith h ts)
    constrainedOk h points = any (constrainedOkWith h) (constrainedTriangulate canvas [h] points)
    constrainedOkWith h ts =
      let edges = concatMap triangleEdges ts
       in all (`elem` edges) (polygonEdges canvas ++ polygonEdges h)
            && all (`isTriangleInPolygon` canvas) ts
            && not (any (`isTriangleInPolygon` h) ts)

-- | The constrained triangulation of a square with a hole, for interior points
-- in general position and for lattice points (which land on the polygon edges,
-- inside the hole, and on top of the corners).
constrainedRegion :: TestTree
constrainedRegion =
  testGroup
    "constrainedTriangulate as a mesh of the region"
    [ testProperty "random points: valid region mesh" $ \(Points points) -> regionHolds hole points
    , testProperty "lattice points: valid region mesh" $ \(GridPoints points) -> regionHolds latticeHole points
    , testProperty "lattice points on the boundary subdivide it" $ \(GridPoints points) ->
        case constrainedTriangulate canvas [latticeHole] points of
          Nothing -> counterexample "Nothing" False
          Just ts ->
            let m = toMesh ts
                onBoundary =
                  [ p
                  | p <- nub points
                  , any (\(Edge u v) -> between u v p) (polygonEdges canvas ++ polygonEdges latticeHole)
                  ]
             in counterexample (show m) $ all (`elem` V.toList (nodes m)) onBoundary
    , testCase "an empty square: two triangles" $
        fmap length (constrainedTriangulate canvas [] []) @?= Just 2
    , testCase "a square with a hole and no interior points: eight triangles" $
        fmap length (constrainedTriangulate canvas [hole] []) @?= Just 8
    , testCase "points inside the hole disappear" $
        fmap
          (length . nodes . toMesh)
          (constrainedTriangulate canvas [hole] [Point 1000 900, Point 900 1000])
          @?= Just 8
    , -- the corner itself belongs to no triangle of the region and disappears
      testCase "a hole touching the boundary at a corner cuts the corner off" $
        let cornerHole = Polygon (Point 0 0 :| [Point 0 500, Point 500 500, Point 500 0])
         in case constrainedTriangulate canvas [cornerHole] [] of
              Nothing -> assertBool "Nothing" False
              Just ts -> do
                length ts @?= 4
                assertBool "locally Delaunay and planar" (isLocallyDelaunay ts && hasNoIntersections ts)
                sort (V.toList (nodes (toMesh ts)))
                  @?= sort [Point 0 500, Point 0 2000, Point 2000 2000, Point 2000 0, Point 500 0, Point 500 500]
                length (boundaryEdges (toMesh ts)) @?= 6
    , testCase "two holes" $
        let hole2 = Polygon (Point 1500 1500 :| [Point 1500 1900, Point 1900 1900, Point 1900 1500])
         in assertBool "valid" $
              any
                (regionOkWith [hole, hole2])
                (constrainedTriangulate canvas [hole, hole2] (concat (deterministicSets 40 1)))
    ]
  where
    canvas = Polygon (Point 0 0 :| [Point 0 2000, Point 2000 2000, Point 2000 0])
    hole = Polygon (Point 600 500 :| [Point 600 1300, Point 1400 1300, Point 1400 500])
    latticeHole = Polygon (Point 600 600 :| [Point 600 1400, Point 1400 1400, Point 1400 600])
    regionHolds h points = case constrainedTriangulate canvas [h] points of
      Nothing -> counterexample "constrainedTriangulate returned Nothing" False
      Just ts -> counterexample (show ts) (regionOk h ts)
    regionOk h = regionOkWith [h]
    regionOkWith holes ts =
      let m = toMesh ts
          polygons = canvas : holes
          boundary = boundaryEdges m
          onPolygonEdge (i, j) =
            let (p, q) = (nodes m V.! i, nodes m V.! j)
             in any (any (\(Edge u v) -> between u v p && between u v q) . polygonEdges) polygons
          nodeCount = V.length (nodes m)
          b = length boundary
       in not (null ts)
            && isLocallyDelaunay ts
            && hasNoIntersections ts
            && not (any isFlat ts)
            && all onPolygonEdge boundary
            && all (all (`elem` V.toList (nodes m)) . vertices) polygons
            && all (`isTriangleInPolygon` canvas) ts
            && not (any (\t -> any (isTriangleInPolygon t) holes) ts)
            && length ts == 2 * nodeCount - b + 2 * length holes - 2
    between u v p =
      orientation u v p == Collinear
        && min (px u) (px v) <= px p
        && px p <= max (px u) (px v)
        && min (py u) (py v) <= py p
        && py p <= max (py u) (py v)

-- | For n points in general position with h of them on the convex hull, any
-- triangulation has exactly 2n - h - 2 triangles. This guards against a
-- triangulation that silently drops triangles: the Delaunay and intersection
-- checks are vacuously true on an empty triangulation.
hasEulerTriangleCount :: Triangulation -> Bool
hasEulerTriangleCount triangulation@(Triangulation (Polygon hullPoints) _) =
  let ts = triangles triangulation
      n = length (nub (concatMap (\(Triangle p1 p2 p3) -> [p1, p2, p3]) ts))
      h = length hullPoints
   in length ts == 2 * n - h - 2

hullContains :: [Point] -> Triangulation -> Bool
hullContains points (Triangulation polygon@(Polygon hullPoints) _) =
  all (\p -> p `elem` hullPoints || isPointInPolygon polygon p) points

wellFormed :: [Point] -> Bool
wellFormed points = case triangulate points of
  Nothing -> False
  Just t ->
    isDelaunay (triangles t)
      && hasNoIntersections (triangles t)
      && hasEulerTriangleCount t
      && isConvex (hull t)

deterministicSets :: Int -> Int -> [[Point]]
deterministicSets size seeds =
  [ generatePoints seed size Rectangle {minCorner = Point 0 0, maxCorner = Point 2000 2000}
  | seed <- [1 .. seeds]
  ]
