{-# OPTIONS -Wall #-}
module Newton where

type R = Double

type Mass = R
type Time = R
type Position = R
type Velocity = R
type Force = R

-- 

integral :: R        -- dt
        -> (R -> R)  -- f(t)
        -> R         -- a (start)
        -> R         -- b (end)
        -> R
integral dt f a b =
    sum [f t * dt | t <- [a+dt/2,a+3*dt/2 .. b-dt/2]]

antiDerivative :: R        -- dt
               -> R        -- f0 (initial value)
               -> (R -> R) -- f' (derivative of f)
               -> (R -> R)
antiDerivative dt f0 f' t = f0 + integral dt f' 0 t

-- 

velocityFt :: R                    -- dt
           -> Mass                 -- m
           -> Velocity             -- v0 (initial velocity)
           -> [Time -> Force]      -- fs (list of forces(t))
           -> (Time -> Velocity)
velocityFt dt m v0 fs =
    let fnet t = sum [f t | f <- fs]
        a t = fnet t / m
    in antiDerivative dt v0 a

positionFt :: R                -- dt
           -> Mass             -- m
           -> Position         -- x0 (initial position)
           -> Velocity         -- v0 (initial velocity)
           -> [Time -> Force]  -- fs (list of forces(t))
           -> (Time -> Position)
positionFt dt m x0 v0 fs =
    antiDerivative dt x0 (velocityFt dt m v0 fs)

fAir :: R        -- drag coefficient 
     -> R        -- rho (air density)
     -> R        -- area (cross section)
     -> Velocity -- v
     -> Force
fAir drag rho area v = - (drag * rho * area * abs v * v / 2)

