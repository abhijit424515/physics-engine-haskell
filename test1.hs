import Newton
import Graphics.Gnuplot.Simple

dt_ :: R
dt_ = 0.1

velocityCF :: Mass -> Velocity -> [Force] -> (Time -> Velocity)
velocityCF m v0 fs = 
    velocityFt dt_ m v0 [\_ -> sum fs]

t1 :: IO ()
t1 =
    plotFunc    [Title "Car"
                ,XLabel "Time (s)"
                ,YLabel "Velocity (m/s)"
                -- ,PNG "CarVelocity.png"
                ,Key Nothing
                ] [0..4 :: Time] (velocityCF 0.1 0.6 [0.04,-0.08])