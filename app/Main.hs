module Main (
  main
) where

import Examples.Simple ( drawSimple )
import Examples.Circle ( drawCircle, drawTorus )
import Examples.Solenoid ( drawSolenoid )

main :: IO ()
main = do
  drawSimple "assets/triangulation_simple.png"
  drawCircle "assets/triangulation_circle.png"
  drawTorus "assets/triangulation_torus.png"
  drawSolenoid "assets/triangulation_solenoid.png"
  return ()
