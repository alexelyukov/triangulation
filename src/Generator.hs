module Generator (
  generatePoints,
  generatePointsWithDistance
) where

import System.Random (mkStdGen, randoms, split)
import Geometry.Types.Point ( Point(..) )
import Geometry.Types.Rectangle ( Rectangle(..) )
import Geometry.Point ( metric )

generatePoints :: Int -> Int -> Rectangle -> [Point]
generatePoints seed n r = take n $ _generatePoints seed r

generatePointsWithDistance :: Int -> Int -> Double -> Rectangle -> [Point] -> [Point]
generatePointsWithDistance seed n distance r existedPoints =
  let points = _generatePoints seed r
      lengthExisted = length existedPoints
  in buildPointsWithDistance (n + lengthExisted) distance points existedPoints

_generatePoints :: Int -> Rectangle -> [Point]
_generatePoints seed r =
  let gen = mkStdGen seed
      (gen1, gen2) = split gen
      randomPointsX = randoms gen1 :: [Double]
      randomPointsY = randoms gen2 :: [Double]
      xs = map (resizeIntoRectangleX r) randomPointsX
      ys = map (resizeIntoRectangleY r) randomPointsY
  in [Point (x, y) | (x, y) <- zip xs ys]

buildPointsWithDistance :: Int -> Double -> [Point] -> [Point] -> [Point]
buildPointsWithDistance _ _ [] out = out
buildPointsWithDistance n distance (inn : inns) out
  | length out == n = out
  | otherwise =
    if checkDistance distance inn out
    then buildPointsWithDistance n distance inns (inn : out)
    else buildPointsWithDistance n distance inns out

checkDistance :: Double -> Point -> [Point] -> Bool
checkDistance _ _ [] = True
checkDistance distance p (q : qs) = metric p q >= distance && checkDistance distance p qs

resizeIntoRectangleX :: Rectangle -> Double -> Double
resizeIntoRectangleX Rectangle { topLeft = Point (x1, _), bottomRight = Point (x2, _) } x = x1 + (x * (x2 - x1))

resizeIntoRectangleY :: Rectangle -> Double -> Double
resizeIntoRectangleY Rectangle { topLeft = Point (_, y1), bottomRight = Point (_, y2) } y = y1 + (y * (y2 - y1))
