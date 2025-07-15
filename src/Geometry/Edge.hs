module Geometry.Edge (
  normalizeEdge,
  calcPointPosition,
  calcCos2,
  getIntersectionPoint
) where

import Geometry.Types.Point ( Point(..) )
import Geometry.Types.PointPosition ( PointPosition(..) )
import Geometry.Types.Edge ( Edge(..) )
import Geometry.Point ( metric )

normalizeEdge :: Edge -> Edge
normalizeEdge (Edge (p1, p2)) = if p1 < p2 then Edge (p1, p2) else Edge (p2, p1)

calcPointPosition :: Edge -> Point -> PointPosition
calcPointPosition (Edge (p1@(Point (x1, y1)), p2@(Point (x2, y2)))) p3@(Point (x3, y3)) =
  let ordering = compare ((x2 - x1) * (y3 - y2)) ((y2 - y1) * (x3 - x2))
      (metric12, metric13) = (metric p1 p2, metric p1 p3)
  in case ordering of
    LT -> PointLeft
    GT -> PointRight
    EQ -> if metric12 > metric13 then PointLeft else PointRight

calcCos2 :: Edge -> Edge -> Double
calcCos2 (Edge (Point (x1, y1), Point (x2, y2))) (Edge (Point (x3, y3), Point (x4, y4))) =
  let (dx1, dy1) = (x2 - x1, y2 - y1)
      (dx2, dy2) = (x4 - x3, y4 - y3)
  in (dx1 * dx2 + dy1 * dy2) ** 2 / ((dx1 * dx1 + dy1 * dy1) * (dx2 * dx2 + dy2 * dy2))

getIntersectionPoint :: Edge -> Edge -> Maybe Point
getIntersectionPoint (Edge (p1@(Point (x1, y1)), p2@(Point (x2, y2))))
                     (Edge (p3@(Point (x3, y3)), p4@(Point (x4, y4)))) =
  let edgesHaveCommonPoints = p1 == p3 || p1 == p4 || p2 == p3 || p2 == p4
      det = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3)
      t = ((x3 - x1) * (y4 - y3) - (y3 - y1) * (x4 - x3)) / det
      u = ((x3 - x1) * (y2 - y1) - (y3 - y1) * (x2 - x1)) / det
      isPointExists = (t >= 0 && t <= 1) && (u >= 0 && u <= 1)
      (xc, yc) = (x1 + t * (x2 - x1), y1 + t * (y2 - y1))
      p0 = Point (xc, yc)
      isPointNotCoincide = all (\p -> metric p0 p >= 10e-5) [p1, p2, p3, p4]
  in if not edgesHaveCommonPoints && det /= 0 && isPointExists && isPointNotCoincide
     then Just p0
     else Nothing
