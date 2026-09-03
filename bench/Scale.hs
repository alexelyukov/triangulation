-- | How the triangulation scales with the number of cores, measured in
-- wall-clock time.
--
-- The @pure-cdt-bench@ benchmarks run on @tasty-bench@, which reports CPU
-- time; that number grows with the number of cores and says nothing about a
-- parallel speedup. This program times the triangulation by the clock
-- instead. Each run gets a freshly generated point set, so no work is shared
-- between runs, and generating the points is not counted.
--
-- > stack bench pure-cdt:pure-cdt-scaling --ba '100000 500000 +RTS -N8'
--
-- With no sizes given it triangulates 50 000 points. Pass @+RTS -N\<k\>@ to
-- choose how many cores to use.
--
-- A leading @lattice@ places the points on a regular lattice instead of
-- drawing them at random, which is the worst case for degeneracy and the
-- likeliest input of a finite element model: every row, column and diagonal
-- is collinear and the corners of every cell are cocircular.
--
-- > stack bench pure-cdt:pure-cdt-scaling --ba 'lattice 100000 +RTS -N8'
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List (sort)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumCapabilities)
import System.Environment (getArgs)
import Text.Printf (printf)
import Triangulation (Point (..), triangles, triangulate)
import Triangulation.Generator (Rectangle (..), generatePoints)

repetitions :: Int
repetitions = 3

main :: IO ()
main = do
  args <- getArgs
  cores <- getNumCapabilities
  let (shape, rest) = case args of
        "lattice" : more -> (Lattice, more)
        more -> (Random, more)
      sizes = case map read rest of
        [] -> [50000]
        given -> given
  printf
    "%s points, cores: %d, %d runs per size, median reported\n"
    (if shape == Lattice then "lattice" else "random")
    cores
    repetitions
  mapM_ (report shape) sizes

-- | Where the points come from.
data Shape = Random | Lattice
  deriving (Eq)

report :: Shape -> Int -> IO ()
report shape n = do
  results <- mapM (timeOne shape n) [1 .. repetitions]
  let times = sort (map fst results)
  case (median times, map snd results) of
    (Just middle, count : _) ->
      printf
        "%8d points -> %8d triangles: %7.0f ms  (runs: %s)\n"
        n
        count
        (middle * 1000)
        (unwords (map (printf "%.0f" . (* 1000)) times))
    _ -> printf "%8d points: no result\n" n

-- | Generate a point set from the seed, then time triangulating it.
timeOne :: Shape -> Int -> Int -> IO (Double, Int)
timeOne shape n seed = do
  points <- evaluate (force (pointsOf shape n seed))
  before <- getMonotonicTime
  result <- evaluate (force (triangulate points))
  count <- evaluate (maybe 0 (length . triangles) result)
  after <- getMonotonicTime
  pure (after - before, count)

median :: [Double] -> Maybe Double
median xs = case drop (length xs `div` 2) xs of
  middle : _ -> Just middle
  [] -> Nothing

-- | @n@ points of the given shape. A lattice ignores the seed: there is only
-- one lattice of a given size, and its regularity is the point.
pointsOf :: Shape -> Int -> Int -> [Point]
pointsOf Random n seed = generatePoints seed n canvas
pointsOf Lattice n _ =
  take n [Point (fromIntegral i * step) (fromIntegral j * step) | i <- [0 .. side], j <- [0 .. side]]
  where
    side = ceiling (sqrt (fromIntegral n :: Double)) :: Int
    step = 300 :: Double

canvas :: Rectangle
canvas = Rectangle {minCorner = Point 0 0, maxCorner = Point 100000 100000}
