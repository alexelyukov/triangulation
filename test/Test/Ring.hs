module Test.Ring (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Triangulation.Geometry.Ring (
  arc,
  cyclicPairs,
  cyclicTriples,
  predecessor,
  splitLoop,
  successor,
 )

ring :: NonEmpty Int
ring = 1 :| [2, 3, 4]

tests :: TestTree
tests =
  testGroup
    "Ring"
    [ testCase "cyclicPairs wraps around" $
        cyclicPairs ring @?= [(1, 2), (2, 3), (3, 4), (4, 1)]
    , testCase "cyclicPairs of a singleton pairs it with itself" $
        cyclicPairs (7 :| [] :: NonEmpty Int) @?= [(7, 7)]
    , testCase "cyclicTriples gives predecessor and successor" $
        cyclicTriples ring @?= [(4, 1, 2), (1, 2, 3), (2, 3, 4), (3, 4, 1)]
    , testCase "successor of the last element is the first" $
        successor 4 ring @?= Just 1
    , testCase "predecessor of the first element is the last" $
        predecessor 1 ring @?= Just 4
    , testCase "neighbours of an absent element" $
        (successor 9 ring, predecessor 9 ring) @?= (Nothing, Nothing)
    , testCase "arc forward" $
        arc 2 4 ring @?= Just (2 :| [3, 4])
    , testCase "arc wrapping around" $
        arc 4 2 ring @?= Just (4 :| [1, 2])
    , testCase "arc from a point to itself goes all the way round" $
        arc 3 3 ring @?= Just (3 :| [4, 1, 2, 3])
    , testCase "arc from an absent point" $
        arc 9 1 ring @?= Nothing
    , testCase "arc to an absent point goes all the way round" $
        arc 1 9 ring @?= Just (1 :| [2, 3, 4, 1])
    , testCase "splitLoop separates the inner loop" $
        splitLoop 2 (1 :| [2, 5, 6, 2, 3] :: NonEmpty Int) @?= (1 :| [2, 3], [2, 5, 6])
    , testCase "splitLoop without a loop" $
        splitLoop 2 ring @?= (ring, [])
    , testCase "splitLoop on an absent element" $
        splitLoop 9 ring @?= (ring, [])
    ]
