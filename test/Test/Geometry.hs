{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Geometry (tests, Points (..)) where

import Data.Hashable (hash)
import Data.List (permutations)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (
  Arbitrary (..),
  Gen,
  choose,
  chooseInt,
  shrinkList,
  testProperty,
  vectorOf,
  (===),
 )
import Triangulation.Geometry

-- | Points inside the 2000×2000 canvas used by the examples.
instance Arbitrary Point where
  arbitrary = Point <$> choose (0, 2000) <*> choose (0, 2000)

-- | A point set of at least three points; shrinking never goes below three.
newtype Points = Points [Point]
  deriving (Show)

instance Arbitrary Points where
  arbitrary = do
    n <- chooseInt (3, 60)
    Points <$> vectorOf n (arbitrary :: Gen Point)
  shrink (Points ps) = [Points ps' | ps' <- shrinkList (const []) ps, length ps' >= 3]

tests :: TestTree
tests =
  testGroup
    "Geometry"
    [ orientationTests
    , exactnessTests
    , edgeTests
    , triangleTests
    , polygonTests
    ]

-- | Triples that are collinear or nearly so, where naive floating-point
-- evaluation gets the sign wrong: @c@ is on the line through @a@ and @b@ up
-- to a perturbation of a few ulps.
newtype NearlyCollinear = NearlyCollinear (Point, Point, Point)
  deriving (Show)

instance Arbitrary NearlyCollinear where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    t <- choose (-1, 2 :: Double)
    ulps <- chooseInt (-3, 3)
    let Point cx cy = Point (px a + t * (px b - px a)) (py a + t * (py b - py a))
    pure $ NearlyCollinear (a, b, Point (nudge ulps cx) cy)
    where
      nudge n x = x + fromIntegral n * (x * 2.220446049250313e-16)

-- | Points on a circle (up to rounding) around a centre, plus one more nearly on it.
newtype NearlyCocircular = NearlyCocircular (Point, Point, Point, Point)
  deriving (Show)

instance Arbitrary NearlyCocircular where
  arbitrary = do
    centre <- arbitrary
    radius <- choose (1, 1000)
    t1 <- choose (0, 2 * pi)
    t2 <- choose (0, 2 * pi)
    t3 <- choose (0, 2 * pi)
    t4 <- choose (0, 2 * pi)
    ulps <- chooseInt (-3, 3)
    let onCircle t = Point (px centre + radius * cos t) (py centre + radius * sin t)
        Point dx dy = onCircle t4
    pure $
      NearlyCocircular
        ( onCircle t1
        , onCircle t2
        , onCircle t3
        , Point (dx + fromIntegral ulps * dx * 2.220446049250313e-16) dy
        )

exactnessTests :: TestTree
exactnessTests =
  testGroup
    "exact predicates"
    [ testProperty "orientation agrees with rational arithmetic on random triples" $
        \a b c -> orientation a b c === referenceOrientation a b c
    , testProperty "orientation agrees with rational arithmetic on nearly collinear triples" $
        \(NearlyCollinear (a, b, c)) -> orientation a b c === referenceOrientation a b c
    , testProperty "in-circle agrees with rational arithmetic on nearly cocircular points" $
        \(NearlyCocircular (a, b, c, d)) ->
          inCircleSign (coordinates a) (coordinates b) (coordinates c) (coordinates d)
            === referenceInCircle a b c d
    ]
  where
    r = toRational
    referenceOrientation (Point ax ay) (Point bx by) (Point cx cy) =
      case compare ((r ax - r cx) * (r by - r cy) - (r ay - r cy) * (r bx - r cx)) 0 of
        LT -> Clockwise
        GT -> CounterClockwise
        EQ -> Collinear
    referenceInCircle (Point ax ay) (Point bx by) (Point cx cy) (Point dx dy) =
      let (adx, ady, bdx, bdy, cdx, cdy) = (r ax - r dx, r ay - r dy, r bx - r dx, r by - r dy, r cx - r dx, r cy - r dy)
       in compare
            ( (adx * adx + ady * ady) * (bdx * cdy - cdx * bdy)
                + (bdx * bdx + bdy * bdy) * (cdx * ady - adx * cdy)
                + (cdx * cdx + cdy * cdy) * (adx * bdy - bdx * ady)
            )
            0

orientationTests :: TestTree
orientationTests =
  testGroup
    "orientation"
    [ testCase "left turn is counter-clockwise (y axis up)" $
        orientation (Point 0 0) (Point 1 0) (Point 1 1) @?= CounterClockwise
    , testCase "right turn is clockwise" $
        orientation (Point 0 0) (Point 1 0) (Point 1 (-1)) @?= Clockwise
    , testCase "points on a line are collinear" $
        orientation (Point 0 0) (Point 1 0) (Point 2 0) @?= Collinear
    , testCase "turn: collinear with the third point closer counts as clockwise" $
        turn (Point 0 0) (Point 2 0) (Point 1 0) @?= Clockwise
    , testCase "turn: collinear with the third point farther counts as counter-clockwise" $
        turn (Point 0 0) (Point 1 0) (Point 2 0) @?= CounterClockwise
    , testProperty "turn never reports collinear" $
        \a b c -> turn a b c /= Collinear
    , testProperty "reversing the walk flips the orientation" $
        \a b c -> orientation a b c === flipOrientation (orientation c b a)
    ]
  where
    flipOrientation Clockwise = CounterClockwise
    flipOrientation CounterClockwise = Clockwise
    flipOrientation Collinear = Collinear

edgeTests :: TestTree
edgeTests =
  testGroup
    "Edge"
    [ testProperty "mkEdge is symmetric" $
        \a b -> mkEdge a b === mkEdge b a
    , testProperty "equal edges hash equally" $
        \a b -> hash (mkEdge a b) === hash (mkEdge b a)
    , testCase "crossing segments intersect at the crossing" $
        intersection (mkEdge (Point 0 0) (Point 2 2)) (mkEdge (Point 0 2) (Point 2 0)) @?= Just (Point 1 1)
    , testCase "parallel segments do not intersect" $
        intersection (mkEdge (Point 0 0) (Point 1 0)) (mkEdge (Point 0 1) (Point 1 1)) @?= Nothing
    , testCase "segments sharing an endpoint do not intersect" $
        intersection (mkEdge (Point 0 0) (Point 1 1)) (mkEdge (Point 0 0) (Point 1 0)) @?= Nothing
    , testCase "segments on crossing lines but apart do not intersect" $
        intersection (mkEdge (Point 0 0) (Point 1 1)) (mkEdge (Point 2 0) (Point 3 1)) @?= Nothing
    , testCase "a segment touching the other at an endpoint does not intersect" $
        intersection (mkEdge (Point 0 0) (Point 2 0)) (mkEdge (Point 1 0) (Point 1 1)) @?= Nothing
    ]

triangleTests :: TestTree
triangleTests =
  testGroup
    "Triangle"
    [ testProperty "mkTriangle ignores the order of the vertices" $
        \a b c -> all (== mkTriangle a b c) [mkTriangle x y z | [x, y, z] <- permutations [a, b, c]]
    , testCase "a point inside the circumcircle" $
        isOutsideCircumcircle (Point 1 1) rightTriangle @?= False
    , testCase "a point far away is outside the circumcircle" $
        isOutsideCircumcircle (Point 5 5) rightTriangle @?= True
    , testCase "a point on the circumcircle counts as outside" $
        isOutsideCircumcircle (Point 2 2) rightTriangle @?= True
    , testCase "isPointInTriangle: inside" $
        isPointInTriangle (mkTriangle (Point 0 0) (Point 4 0) (Point 0 4)) (Point 1 1) @?= True
    , testCase "isPointInTriangle: outside" $
        isPointInTriangle (mkTriangle (Point 0 0) (Point 4 0) (Point 0 4)) (Point 3 3) @?= False
    , testCase "isPointInTriangle: a vertex is not inside" $
        isPointInTriangle (mkTriangle (Point 0 0) (Point 4 0) (Point 0 4)) (Point 4 0) @?= False
    , testCase "triangles are sorted into and out of a polygon by their centroid" $
        let ts =
              [ mkTriangle (Point 1 1) (Point 2 1) (Point 1 2)
              , mkTriangle (Point 10 10) (Point 11 10) (Point 10 11)
              ]
         in (trianglesInside square ts, trianglesOutside square ts) @?= splitAt 1 ts
    ]
  where
    rightTriangle = mkTriangle (Point 0 0) (Point 2 0) (Point 0 2)

-- | The unit square scaled to 4, wound clockwise.
square :: Polygon
square = Polygon (Point 0 0 :| [Point 0 4, Point 4 4, Point 4 0])

polygonTests :: TestTree
polygonTests =
  testGroup
    "Polygon"
    [ testCase "hullOf3 starts at the lower-left point and winds clockwise" $
        vertices (hullOf3 (Point 2 0) (Point 1 1) (Point 0 0)) @?= [Point 0 0, Point 1 1, Point 2 0]
    , testProperty "hullOf3 is convex and clockwise" $
        \a b c -> isConvex (hullOf3 a b c)
    , testCase "hullOf4 of a square keeps all four corners" $
        case hullOf4 (Point 2 2) (Point 0 0) (Point 2 0) (Point 0 2) of
          Quadrilateral p1 p2 p3 p4 -> [p1, p2, p3, p4] @?= [Point 0 0, Point 0 2, Point 2 2, Point 2 0]
          TriangleWithInner {} -> assertBool "expected a quadrilateral" False
    , testCase "hullOf4 finds the inner point" $
        case hullOf4 (Point 0 0) (Point 2 1) (Point 4 0) (Point 2 4) of
          TriangleWithInner _ _ _ inner -> inner @?= Point 2 1
          Quadrilateral {} -> assertBool "expected a triangle with an inner point" False
    , testProperty "hullOf4 is convex and clockwise" $
        \a b c d -> isConvex (hull4Polygon (hullOf4 a b c d))
    , testCase "isPointInPolygon: inside" $ isPointInPolygon square (Point 1 1) @?= True
    , testCase "isPointInPolygon: outside" $ isPointInPolygon square (Point 5 5) @?= False
    , testCase "isPointInPolygon: a vertex is not inside" $ isPointInPolygon square (Point 4 4) @?= False
    , testCase "a square is convex" $ isConvex square @?= True
    , testCase "a polygon with a reflex vertex is not convex" $
        isConvex (Polygon (Point 0 0 :| [Point 0 3, Point 3 3, Point 1 2])) @?= False
    , testCase "polygonEdges closes the ring" $
        length (polygonEdges square) @?= 4
    ]
