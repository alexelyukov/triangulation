module Test.Refine (tests) where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Vector qualified as V
import Test.Geometry (Points (..))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (Property, QuickCheckTests (..), counterexample, testProperty, (===))
import Triangulation
import Triangulation.Check (hasNoIntersections, isLocallyDelaunay)
import Triangulation.Generator (Rectangle (..), generatePoints)

tests :: TestTree
tests =
  testGroup
    "Refine"
    [ localOption (QuickCheckTests 100) properties
    , shapes
    , edgeCases
    ]

properties :: TestTree
properties =
  testGroup
    "random interior points in a square with a hole"
    [ testProperty "20°: every angle is at least 20° and the mesh is valid" $
        refined defaultQuality [canvas, hole] $
          \ts -> wellRefined defaultQuality [canvas, hole] ts
    , testProperty "20° and an area bound: no triangle is larger than the bound" $
        let q = defaultQuality {maxArea = Just 20000}
         in refined q [canvas, hole] $ \ts -> wellRefined q [canvas, hole] ts
    , testProperty "the input points survive refinement" $
        \(Points points) -> case constrainedTriangulate canvas [hole] points >>= refine defaultQuality of
          Nothing -> counterexample "Nothing" False
          Just ts ->
            let ns = V.toList (nodes (toMesh ts))
                kept = filter (\p -> strictlyInside canvas p && not (strictlyInside hole p)) points
             in counterexample (show ns) $ all (`elem` ns) (kept ++ vertices canvas ++ vertices hole)
    , testProperty "refining twice changes nothing" $
        \(Points points) -> case constrainedTriangulate canvas [hole] points >>= refine defaultQuality of
          Nothing -> counterexample "Nothing" False
          Just ts -> fmap sort (refine defaultQuality ts) === Just (sort ts)
    , testProperty "the Delaunay triangulation of a point set (hull as boundary) refines too" $
        -- the corners keep the hull free of acute angles, which Ruppert's algorithm cannot fix
        \(Points points) -> case triangulate (vertices canvas ++ points) of
          Nothing -> counterexample "triangulate returned Nothing" False
          Just t -> case refine defaultQuality (triangles t) of
            Nothing -> counterexample "refine returned Nothing" False
            Just ts -> counterexample (show ts) (wellRefined defaultQuality [hull t] ts)
    ]

-- | The refinement of the constrained triangulation of random points must
-- succeed and satisfy the predicate.
refined :: Quality -> [Polygon] -> ([Triangle] -> Bool) -> Points -> Property
refined quality (boundary : holes) p (Points points) =
  case constrainedTriangulate boundary holes points of
    Nothing -> counterexample "constrainedTriangulate returned Nothing" False
    Just coarse -> case refine quality coarse of
      Nothing -> counterexample "refine returned Nothing" False
      Just ts -> counterexample (show ts) (p ts)
refined _ [] _ _ = counterexample "no boundary" False

shapes :: TestTree
shapes =
  testGroup
    "particular shapes"
    [ testCase "an L-shaped region at 30°" $
        check (Just q30) [lShape] (constrainedTriangulate lShape [] (grid 300) >>= refine q30)
    , testCase "a square with a hole at 30° and an area bound" $
        let q = q30 {maxArea = Just 50000}
         in check (Just q) [canvas, hole] (constrainedTriangulate canvas [hole] (grid 500) >>= refine q)
    , testCase "a wedge with a 17° input angle terminates and keeps its boundary" $
        case constrainedTriangulate wedge [] [] >>= refine defaultQuality of
          Nothing -> assertBool "refine returned Nothing" False
          Just ts -> do
            problems Nothing [wedge] ts @?= []
            -- every angle away from the wedge apex meets the bound
            assertBool "angles away from the apex" $
              all (\t -> smallestAngle t >= 20 - tolerance || touches (Point 0 0) t) ts
    , testCase "a wedge with a 17° input angle at 30° with an area bound" $
        check Nothing [wedge] (constrainedTriangulate wedge [] [] >>= refine q30 {maxArea = Just 5000})
    , testCase "a thin rectangle at 30°" $
        let thin = Polygon (Point 0 0 :| [Point 0 100, Point 2000 100, Point 2000 0])
         in check (Just q30) [thin] (constrainedTriangulate thin [] [] >>= refine q30)
    , testCase "a hexagon with a triangular hole"
        $ check (Just defaultQuality) [hexagon, triangleHole]
        $ constrainedTriangulate hexagon [triangleHole] [] >>= refine defaultQuality
    , testCase "1000 random points at 25°" $
        let q = defaultQuality {minAngle = 25}
            points = generatePoints 3 1000 Rectangle {minCorner = Point 0 0, maxCorner = Point 2000 2000}
         in check (Just q) [canvas, hole] (constrainedTriangulate canvas [hole] points >>= refine q)
    ]
  where
    q30 = defaultQuality {minAngle = 30}
    grid h = [Point x y | x <- [h, 2 * h .. 2000 - h], y <- [h, 2 * h .. 2000 - h]]
    check _ _ Nothing = assertBool "refine returned Nothing" False
    check quality polygons (Just ts) = problems quality polygons ts @?= []

edgeCases :: TestTree
edgeCases =
  testGroup
    "edge cases"
    [ testCase "no triangles" $ refine defaultQuality [] @?= Just []
    , testCase "a triangle that already meets the bound is returned as is" $
        let t = mkTriangle (Point 0 0) (Point 100 0) (Point 50 80)
         in refine defaultQuality [t] @?= Just [t]
    , testCase "no budget and a poor triangle give Nothing" $
        let t = mkTriangle (Point 0 0) (Point 1000 0) (Point 500 10)
         in assertBool "expected Nothing" $ isNothing (refine defaultQuality {maxInsertions = 0} [t])
    , testCase "no budget and a good triangle succeed" $
        let t = mkTriangle (Point 0 0) (Point 100 0) (Point 50 80)
         in refine defaultQuality {maxInsertions = 0} [t] @?= Just [t]
    , testCase "an impossible bound exhausts the budget rather than looping"
        $
        -- a square cut by its diagonal: two 45° angles at interior edges, which no refinement lifts to 59°
        assertBool "expected Nothing"
        $ isNothing (constrainedTriangulate canvas [] [] >>= refine (Quality 59 Nothing 200))
    , testCase "an input angle is not asked to meet the bound" $
        let t = mkTriangle (Point 0 0) (Point 100 0) (Point 50 80)
         in refine (Quality 59 Nothing 200) [t] @?= Just [t]
    , testCase "no bounds at all: only encroached segments are split" $
        -- the apex sees the long edge at 118°, so it encroaches upon it
        let t = mkTriangle (Point 0 0) (Point 1000 0) (Point 500 300)
         in case refine (Quality 0 Nothing 100) [t] of
              Nothing -> assertBool "Nothing" False
              Just ts -> do
                problems Nothing [Polygon (Point 0 0 :| [Point 500 300, Point 1000 0])] ts @?= []
                assertBool "something was split" (length ts > 1)
    ]

-- Shapes; polygons are wound clockwise (y up: interior on the right).

canvas, hole, lShape, wedge, hexagon, triangleHole :: Polygon
canvas = Polygon (Point 0 0 :| [Point 0 2000, Point 2000 2000, Point 2000 0])
hole = Polygon (Point 600 500 :| [Point 600 1300, Point 1400 1300, Point 1400 500])
lShape =
  Polygon
    (Point 0 0 :| [Point 0 2000, Point 1000 2000, Point 1000 1000, Point 2000 1000, Point 2000 0])
wedge = Polygon (Point 0 0 :| [Point 0 1000, Point 300 1000])
hexagon =
  Polygon
    (Point 1000 0 :| [Point 134 500, Point 134 1500, Point 1000 2000, Point 1866 1500, Point 1866 500])
triangleHole = Polygon (Point 700 700 :| [Point 1000 1300, Point 1300 700])

tolerance :: Double
tolerance = 1e-9

-- | The mesh satisfies the bounds and is a valid triangulation of the region.
wellRefined :: Quality -> [Polygon] -> [Triangle] -> Bool
wellRefined quality polygons ts = null (problems (Just quality) polygons ts)

-- | What is wrong with the mesh: bounds not met (when a quality is given), or
-- not a constrained Delaunay triangulation of exactly the region bounded by
-- the polygons (the first is the outer boundary, the rest are holes).
problems :: Maybe Quality -> [Polygon] -> [Triangle] -> [String]
problems quality polygons ts =
  [ name
  | (name, ok) <-
      [
        ( "an angle below the bound"
        , all (\q -> all (\t -> smallestAngle t >= minAngle q - tolerance) ts) quality
        )
      ,
        ( "an area above the bound"
        , all (\a -> all (\t -> triangleArea t <= a) ts) (quality >>= maxArea)
        )
      , ("no triangles", not (null ts))
      , ("not locally Delaunay", isLocallyDelaunay ts)
      , ("crossing edges", hasNoIntersections ts)
      , ("a flat triangle", all (\(Triangle a b c) -> orientation a b c /= Collinear) ts)
      , ("a boundary edge off the polygons", boundaryOnPolygons)
      , ("a polygon vertex missing", all (all (`elem` V.toList (nodes m)) . vertices) polygons)
      , ("Euler's formula", eulerWithHoles)
      , ("an encroached segment", noEncroachedSegments m)
      ]
  , not ok
  ]
  where
    m = toMesh ts
    boundary = boundaryEdges m
    -- every boundary edge of the mesh lies on an edge of one of the polygons
    boundaryOnPolygons = all onSomePolygonEdge boundary
    onSomePolygonEdge (i, j) =
      let (p, q) = (nodes m V.! i, nodes m V.! j)
       in any (any (\(Edge u v) -> between u v p && between u v q) . polygonEdges) polygons
    -- split points are computed in floating point, so "on the edge" is up to rounding
    between u v p =
      let (dx, dy) = (px v - px u, py v - py u)
          len2 = dx * dx + dy * dy
          t = ((px p - px u) * dx + (py p - py u) * dy) / len2
          cross = dx * (py p - py u) - dy * (px p - px u)
       in -1e-9 <= t && t <= 1 + 1e-9 && cross * cross <= 1e-18 * len2 * len2
    -- for a triangulated region with V vertices, B boundary vertices and H holes: T = 2V - B + 2H - 2
    eulerWithHoles =
      let v = V.length (nodes m)
          b = length boundary -- boundary loops have as many edges as vertices
          h = length polygons - 1
       in V.length (elements m) == 2 * v - b + 2 * h - 2

-- | No vertex lies strictly inside the diametral circle of a boundary edge.
noEncroachedSegments :: Mesh -> Bool
noEncroachedSegments m =
  and
    [ not (encroaches p (nodes m V.! i) (nodes m V.! j))
    | (i, j) <- boundaryEdges m
    , p <- V.toList (nodes m)
    ]
  where
    encroaches (Point x y) (Point x1 y1) (Point x2 y2) = (x - x1) * (x - x2) + (y - y1) * (y - y2) < 0

touches :: Point -> Triangle -> Bool
touches p (Triangle a b c) = p `elem` [a, b, c]

-- | Strictly inside an axis-aligned rectangle given as a polygon.
strictlyInside :: Polygon -> Point -> Bool
strictlyInside polygon (Point x y) =
  let xs = map px (vertices polygon)
      ys = map py (vertices polygon)
   in minimum xs < x && x < maximum xs && minimum ys < y && y < maximum ys
