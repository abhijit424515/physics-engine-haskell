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

pedalCoastAir2 :: Time -> Velocity
pedalCoastAir2 = velocityFtv 0.1 20 (0,0)
                [\(t,_) -> ff t
                ,\(_,v) -> fAir 2 1.225 0.5 v]

t5 :: IO ()
t5 =
    plotFunc [Title "Pedaling and coasting with air"
             ,XLabel "Time (s)"
             ,YLabel "Velocity of Bike (m/s)"
            --  ,PNG "pedalCoastAir2Graph.png"
             ,Key Nothing
             ] [0,0.5..100 :: R] pedalCoastAir2