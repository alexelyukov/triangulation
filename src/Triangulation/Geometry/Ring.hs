-- | A non-empty list viewed as a ring: the successor of the last element is the
-- first one. Polygon vertex lists are rings.
module Triangulation.Geometry.Ring (
  cyclicPairs,
  cyclicTriples,
  successor,
  predecessor,
  arc,
  splitLoop,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Tuple (swap)

-- | Every element paired with its successor.
cyclicPairs :: NonEmpty a -> [(a, a)]
cyclicPairs (x :| xs) = zip (x : xs) (xs ++ [x])

-- | Every element with its neighbours: @(predecessor, element, successor)@.
cyclicTriples :: NonEmpty a -> [(a, a, a)]
cyclicTriples ring =
  let xs = NE.toList ring
      n = length xs
   in zip3 (drop (n - 1) xs ++ take (n - 1) xs) xs (drop 1 xs ++ take 1 xs)

-- | Successor of the first occurrence of an element.
successor :: Eq a => a -> NonEmpty a -> Maybe a
successor x = lookup x . cyclicPairs

-- | Predecessor of the first occurrence of an element.
predecessor :: Eq a => a -> NonEmpty a -> Maybe a
predecessor x = lookup x . map swap . cyclicPairs

-- | The arc from @from@ to @to@, walking forward and wrapping around, both ends
-- included. When @from == to@ the arc goes all the way round and ends at
-- @from@ again; the same happens when @to@ is absent. 'Nothing' when @from@
-- is not on the ring.
arc :: Eq a => a -> a -> NonEmpty a -> Maybe (NonEmpty a)
arc from to ring = case break (== from) (NE.toList ring) of
  (_, []) -> Nothing
  (before, x : after) -> Just (x :| takeThrough (== to) (after ++ before ++ [x]))

takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough p xs = let (prefix, rest) = break p xs in prefix ++ take 1 rest

-- | Split a ring that visits @x@ twice into the outer ring, which visits @x@
-- once, and the inner loop between the two visits, which starts with @x@.
-- A ring visiting @x@ at most once is returned unchanged with an empty loop.
splitLoop :: Eq a => a -> NonEmpty a -> (NonEmpty a, [a])
splitLoop x ring = case break (== x) (NE.toList ring) of
  (_, []) -> (ring, [])
  (before, _ : rest) ->
    case break (== x) (reverse rest) of
      (_, []) -> (ring, [])
      (afterRev, _ : innerRev) -> (NE.prependList before (x :| reverse afterRev), x : reverse innerRev)
