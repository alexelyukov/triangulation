module TriangulationStore (
  TriangulationStore(..),
  hasTrianglesOnEdge,
  getTriangles,
  getEdgesFromStore,
  insertTriangle,
  deleteTriangle,
  getAllTriangles,
  unionTriangulationStores,
  emptyTriangulationStore
) where

import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData, rnf)
import Geometry.Types.Polygon ( Polygon(..) )
import Geometry.Types.Edge ( Edge(..) )
import Geometry.Types.Triangle ( Triangle(..) )
import Geometry.Polygon ( buildConvexHull )

newtype TriangulationStore = TriangulationStore (HM.HashMap Edge [Triangle]) deriving (Show)

instance NFData TriangulationStore where
  rnf (TriangulationStore store) = rnf store

emptyTriangulationStore :: TriangulationStore
emptyTriangulationStore = TriangulationStore HM.empty

unionTriangulationStores :: TriangulationStore -> TriangulationStore -> TriangulationStore
unionTriangulationStores (TriangulationStore store1) (TriangulationStore store2) =
  TriangulationStore $ HM.union store1 store2

hasTrianglesOnEdge :: Edge -> TriangulationStore -> Bool
hasTrianglesOnEdge edge (TriangulationStore store) =
  let normalizedEdge = normalizeEdge edge
  in HM.member normalizedEdge store

getTriangles :: Edge -> TriangulationStore -> [Triangle]
getTriangles edge (TriangulationStore store) =
  let normalizedEdge = normalizeEdge edge
  in fromMaybe [] $ HM.lookup normalizedEdge store

getEdgesFromStore :: TriangulationStore -> [Edge]
getEdgesFromStore (TriangulationStore store) = HM.keys store

insertTriangle :: Triangle -> TriangulationStore -> TriangulationStore
insertTriangle triangle (TriangulationStore store) =
  let edges = map normalizeEdge $ getEdges triangle
      normalizedTriangle = normalizeTriangle triangle
      insertFn store' key =
        let existingTriangles = fromMaybe [] $ HM.lookup key store'
            savedTriangles = nub $ normalizedTriangle : existingTriangles
        in HM.insert key savedTriangles store'
  in TriangulationStore $ foldl insertFn store edges

deleteTriangle :: Triangle -> TriangulationStore -> TriangulationStore
deleteTriangle triangle (TriangulationStore store) =
  let edges = map normalizeEdge $ getEdges triangle
      deleteFn store' key =
        let existingTriangles = fromMaybe [] $ HM.lookup key store'
            savedTriangles = filter (/= triangle) existingTriangles
        in if null savedTriangles
           then HM.delete key store'
           else HM.insert key savedTriangles store'
  in TriangulationStore $ foldl deleteFn store edges

getAllTriangles :: TriangulationStore -> [Triangle]
getAllTriangles (TriangulationStore store) = (nub . concat) $ HM.elems store

getEdges :: Triangle -> [Edge]
getEdges (Triangle (p1, p2, p3)) = [Edge (p1, p2), Edge (p2, p3), Edge (p3, p1)]

normalizeTriangle :: Triangle -> Triangle
normalizeTriangle (Triangle (p1, p2, p3)) =
  let convexHull = buildConvexHull [p1, p2, p3]
      Polygon convexHullPoints = convexHull
  in Triangle (convexHullPoints !! 0, convexHullPoints !! 1, convexHullPoints !! 2)

normalizeEdge :: Edge -> Edge
normalizeEdge (Edge (p1, p2)) = if p1 < p2 then Edge (p1, p2) else Edge (p2, p1)
