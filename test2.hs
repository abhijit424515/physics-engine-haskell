import Newton
import Graphics.Gnuplot.Simple

ff :: Time -> Force
ff t = 
    let tCycle = 20
        nComplete :: Int
        nComplete = truncate (t / tCycle)
        remainder = t - fromIntegral nComplete * tCycle
    in if remainder < 10
       then 10
       else 0

t2 :: IO ()
t2 =
    plotFunc [Title "Child pedaling then coasting"
             ,XLabel "Time (s)"
             ,YLabel "Position of Bike (m)"
             -- ,PNG "ChildPosition.png"
             ,Key Nothing
             ] [0..40 :: R] (positionFt 0.1 20 0 0 [ff])