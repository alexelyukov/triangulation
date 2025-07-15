module Utils (
  rebuildBy2Elements,
  splitByElement,
  predOfFirstElement,
  predOfLastElement,
  succOfFirstElement,
  succOfLastElement,
  removeSeqDouble
) where

import Data.List ( elemIndices )

-- lists must be non-empty, elements must be present in the lists for all functions

rebuildBy2Elements :: Eq a => [a] -> a -> a -> [a]
rebuildBy2Elements list elem1 elem2 =
  let index1 = getFirstIndex elem1 list
      index2 = getFirstIndex elem2 list
  in if index1 < index2
     then drop index1 $ take (index2 + 1) list
     else drop index1 list ++ take (index2 + 1) list

splitByElement :: Eq a => [a] -> a -> ([a], [a])
splitByElement list elem' =
  let firstIndex = getFirstIndex elem' list
      lastIndex = getLastIndex elem' list
      left1 = take (firstIndex + 1) list
      left2 = drop (lastIndex + 1) list
      right = drop firstIndex $ take lastIndex list
  in (left1 ++ left2, right)

predOfFirstElement :: Eq a => [a] -> a -> a
predOfFirstElement list element = predElement list (getFirstIndex element list)

predOfLastElement :: Eq a => [a] -> a -> a
predOfLastElement list element = predElement list (getLastIndex element list)

succOfFirstElement :: Eq a => [a] -> a -> a
succOfFirstElement list element = succElement list (getFirstIndex element list)

succOfLastElement :: Eq a => [a] -> a -> a
succOfLastElement list element = succElement list (getLastIndex element list)

getFirstIndex :: Eq a => a -> [a] -> Int
getFirstIndex element list = head $ elemIndices element list

getLastIndex :: Eq a => a -> [a] -> Int
getLastIndex element list = last $ elemIndices element list

predElement :: [a] -> Int -> a
predElement list 0 = last list
predElement list index = list !! (index - 1)

succElement :: [a] -> Int -> a
succElement list index
  | index == length list - 1 = head list
  | otherwise = list !! (index + 1)

removeSeqDouble :: Eq a => [a] -> [a]
removeSeqDouble [] = []
removeSeqDouble [x] = [x]
removeSeqDouble (x1:x2:xs)
  | x1 == x2    = removeSeqDouble (x2:xs)
  | otherwise = x1 : removeSeqDouble (x2:xs)
