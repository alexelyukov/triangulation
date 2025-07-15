module TriangulationEvaluation (
  buildTEvaluation,
  evalTriangulation
) where

import Data.List (sortBy)
import Control.Monad.Par ( fork, get, new, put, runPar, IVar, Par )
import Geometry.Types.Partition ( Partition(..) )
import Geometry.Types.Point ( Point(..) )
import Geometry.Point ( getLeftTopPoint, getRightTopPoint, getBottomRightPoint, getTopRightPoint )
import Triangulation ( Triangulation, triangulate, mergeTriangulations )

type TEvaluation = Par (IVar Triangulation)

buildTEvaluation :: [Point] -> TEvaluation
buildTEvaluation points = buildTEvaluationTree (calcPartition points) points

evalTriangulation :: TEvaluation -> Triangulation
evalTriangulation evaluation = runPar $ do i <- evaluation; get i

buildTEvaluationTree :: Partition -> [Point] -> TEvaluation
buildTEvaluationTree partition points
  | length points `elem` [3, 4] = do i <- new; fork (put i $ triangulate points); return i
  | otherwise = divideTriangulationTree points partition

divideTriangulationTree :: [Point] -> Partition -> TEvaluation
divideTriangulationTree points partition =
  let (leftPoints, rightPoints) = if length points `elem` [9, 10, 11]
        then splitBy3 points partition
        else splitByHalf points partition
      newPartition = calcPartition points
      (leftNode, rightNode) = runPar $ do
        l <- new; r <- new
        fork (put l $ evalTriangulation $ buildTEvaluationTree newPartition leftPoints)
        fork (put r $ evalTriangulation $ buildTEvaluationTree newPartition rightPoints)
        l' <- get l; r' <- get r
        return (l', r')
  in do
    i <- new
    fork ( put i $ mergeTriangulations leftNode rightNode partition)
    return i

splitByHalf :: [Point] -> Partition -> ([Point], [Point])
splitByHalf points partition =
  let sortedPoints = sortBy (pointsSortFn partition) points
      lengthHalf = length sortedPoints `div` 2
  in splitAt lengthHalf sortedPoints

splitBy3 :: [Point] -> Partition -> ([Point], [Point])
splitBy3 points partition =
  let sortedPoints = sortBy (pointsSortFn partition) points
  in splitAt 3 sortedPoints

pointsSortFn :: Partition -> (Point -> Point -> Ordering)
pointsSortFn Vertical = \(Point (x1, _)) (Point (x2, _)) -> compare x1 x2
pointsSortFn Horizontal = \(Point (_, y1)) (Point (_, y2)) -> compare y2 y1

calcPartition :: [Point] -> Partition
calcPartition points =
  let Point (xmin, _) = getLeftTopPoint points
      Point (xmax, _) = getRightTopPoint points
      Point (_, ymin) = getBottomRightPoint points
      Point (_, ymax) = getTopRightPoint points
  in if xmax - xmin > ymax - ymin then Vertical else Horizontal
