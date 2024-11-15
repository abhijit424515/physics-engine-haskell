import Newton
import Graphics.Gnuplot.Simple

bikeVelocity :: Time -> Velocity 
bikeVelocity = velocityFv 1 70 0 [const 100,fAir 2 1.225 0.6]

t3 :: IO ()
t3 =
    plotFunc [Title "Bike Velocity"
             ,XLabel "Time (s)"
             ,YLabel "Velocity of Bike (m/s)"
             -- ,PNG "BikeVelocity1.png"
             ,Key Nothing
             ] [0,0.5..60 :: R] bikeVelocity