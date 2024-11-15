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

pedalCoastAir :: [(Time,Velocity)] 
pedalCoastAir = statesTV 0.1 20 (0,0)
                [\(t,_) -> ff t
                ,\(_,v) -> fAir 2 1.225 0.5 v]

t4 :: IO ()
t4 =
    plotPath [Title "Pedaling and coasting with air"
             ,XLabel "Time (s)"
             ,YLabel "Velocity of Bike (m/s)"
            --  ,PNG "pedalCoastAirGraph.png"
             ,Key Nothing
             ] (takeWhile (\(t,_) -> t <= 100) pedalCoastAir)