module Main (
  main,
) where

import Examples.Circle (drawCircle, drawTorus)
import Examples.Gear (drawGear)
import Examples.Simple (drawSimple)
import Examples.Solenoid (drawRefinedSolenoid, drawSolenoid)

main :: IO ()
main = do
  drawGear "assets/gear.png"
  drawSimple "assets/triangulation_simple.png"
  drawCircle "assets/triangulation_circle.png"
  drawTorus "assets/triangulation_torus.png"
  drawSolenoid "assets/triangulation_solenoid.png"
  drawRefinedSolenoid "assets/triangulation_refined.png"
  return ()
