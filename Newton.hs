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

newtonSecondV :: Mass                -- m
              -> [Velocity -> Force] -- fs (list of forces(v)) 
              -> Velocity            -- v0 (current velocity)
              -> R                   -- v' (derivative of velocity)
newtonSecondV m fs v0 = sum [f v0 | f <- fs] / m

updateVelocity :: R                    -- dt
               -> Mass                 -- m
               -> [Velocity -> Force]  -- fs (list of forces(v))
               -> Velocity             -- v0 (current velocity)
               -> Velocity             -- v1 (new velocity)
updateVelocity dt m fs v0 = v0 + dt * newtonSecondV m fs v0

velocityFv :: R                    -- dt
           -> Mass                 -- m
           -> Velocity             -- v0 (initial velocity)
           -> [Velocity -> Force]  -- fs (list of forces(v))
           -> (Time -> Velocity)   -- v(t)
velocityFv dt m v0 fs t = 
    let steps = abs $ round (t/dt)
    in iterate (updateVelocity dt m fs) v0 !! steps

-- \frac{d (t,v(t))}{dt} = (1,\frac{\sum_j {F_j(t,v(t))}}{m})
newtonSecondTV :: Mass                        -- m
               -> [(Time,Velocity) -> Force]  -- fs (list of forces(t,v))
               -> (Time,Velocity)             -- (t,v0) current state
               -> (R,R)                       -- (1,acceleration)
newtonSecondTV m fs (t,v0) =
    let fnet = sum [f (t,v0) | f <- fs]
        a = fnet/m
    in (1,a)

-- v(t+\delta{t}) = v(t) + \delta{t} \frac{F_{net}(t,v(t))}{m}

updateTV :: R                            -- dt
         -> Mass                         -- m
         -> [(Time,Velocity) -> Force]   -- fs (list of forces(t,v))
         -> (Time,Velocity)              -- current state
         -> (Time,Velocity)              -- new state
updateTV dt m fs (t,v0) =
    let (dt_dt, dv_dt) = newtonSecondTV m fs (t,v0)
    in (t + dt*dt_dt, v0 + dt*dv_dt)

-- 

statesTV :: R                            -- dt
         -> Mass                         -- m
         -> (Time,Velocity)              -- tv0 (initial state)
         -> [(Time,Velocity) -> Force]   -- fs (list of forces(t,v))
         -> [(Time,Velocity)]            -- inf list of states
statesTV dt m tv0 fs =
    iterate (updateTV dt m fs) tv0

velocityFtv :: R                            -- dt
            -> Mass                         -- m
            -> (Time,Velocity)              -- tv0 (initial state)
            -> [(Time,Velocity) -> Force]   -- fs (list of forces(t,v))
            -> (Time -> Velocity)           -- f(v)
velocityFtv dt m tv0 fs t =
    let steps = abs $ round (t/dt)
    in snd $ statesTV dt m tv0 fs !! steps 