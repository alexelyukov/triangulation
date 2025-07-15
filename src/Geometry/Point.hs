module Geometry.Point (
  metric,
  isPointNotInCircle,
  getLeftPoints,
  getBottomLeftPoint,
  getBottomRightPoint,
  getLeftBottomPoint,
  getLeftTopPoint,
  getTopLeftPoint,
  getTopRightPoint,
  getRightBottomPoint,
  getRightTopPoint
) where

import Geometry.Types.Point ( Point(..) )
import Geometry.Types.Triangle ( Triangle(..) )
import Data.Matrix (fromLists, detLU)

metric :: Point -> Point -> Double
metric (Point (x1, y1)) (Point (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

isPointNotInCircle :: Point -> Triangle -> Bool
isPointNotInCircle (Point (x0, y0)) (Triangle (Point (x1, y1), Point (x2, y2), Point (x3, y3))) =
  let (xy02, xy12, xy22, xy32) = (x0 ** 2 + y0 ** 2, x1 ** 2 + y1 ** 2, x2 ** 2 + y2 ** 2, x3 ** 2 + y3 ** 2)
      aMatrix = fromLists [
        [x1, y1, 1],
        [x2, y2, 1],
        [x3, y3, 1]]
      bMatrix = fromLists [
        [xy12, y1, 1],
        [xy22, y2, 1],
        [xy32, y3, 1]]
      cMatrix = fromLists [
        [xy12, x1, 1],
        [xy22, x2, 1],
        [xy32, x3, 1]]
      dMatrix = fromLists [
        [xy12, x1, y1],
        [xy22, x2, y2],
        [xy32, x3, y3]]
      (aDet, bDet, cDet, dDet) = (detLU aMatrix, detLU bMatrix, detLU cMatrix, detLU dMatrix)
      condition = aDet * xy02 - bDet * x0 + cDet * y0 - dDet
  in if aDet > 0 then condition >= 0 else condition < 0

getBottomLeftPoint :: [Point] -> Point
getBottomLeftPoint = packPoint . getLeftPoints . getBottomPoints

getBottomRightPoint :: [Point] -> Point
getBottomRightPoint = packPoint . getRightPoints . getBottomPoints

getLeftBottomPoint :: [Point] -> Point
getLeftBottomPoint = packPoint . getBottomPoints . getLeftPoints

getLeftTopPoint :: [Point] -> Point
getLeftTopPoint = packPoint . getTopPoints . getLeftPoints

getTopLeftPoint :: [Point] -> Point
getTopLeftPoint = packPoint . getLeftPoints . getTopPoints

getTopRightPoint :: [Point] -> Point
getTopRightPoint = packPoint . getRightPoints . getTopPoints

getRightBottomPoint :: [Point] -> Point
getRightBottomPoint = packPoint . getBottomPoints . getRightPoints

getRightTopPoint :: [Point] -> Point
getRightTopPoint = packPoint . getTopPoints . getRightPoints

getLeftPoints :: [Point] -> [Point]
getLeftPoints points =
  let (Point (minX, _), _) = getBoundingBox points
  in filter (\(Point (x, _)) -> x == minX) points

getBottomPoints :: [Point] -> [Point]
getBottomPoints points =
  let (Point (_, minY), _) = getBoundingBox points
  in filter (\(Point (_, y)) -> y == minY) points

getRightPoints :: [Point] -> [Point]
getRightPoints points =
  let (_, Point (maxX, _)) = getBoundingBox points
  in filter (\(Point (x, _)) -> x == maxX) points

getTopPoints :: [Point] -> [Point]
getTopPoints points =
  let (_, Point (_, maxY)) = getBoundingBox points
  in filter (\(Point (_, y)) -> y == maxY) points

getBoundingBox :: [Point] -> (Point, Point)
getBoundingBox points =
  let xs = map (\(Point (x, _)) -> x) points
      ys = map (\(Point (_, y)) -> y) points
      (minX, minY) = (minimum xs, minimum ys)
      (maxX, maxY) = (maximum xs, maximum ys)
  in (Point (minX, minY), Point (maxX, maxY))

packPoint :: [Point] -> Point
packPoint [] = error "No point found"
packPoint (p:_) = p
