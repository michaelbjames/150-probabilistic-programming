module Examples.LinearRegression where

import Bayes

{-
Train
Guess
-}

train :: [(x,y)] -> Bayes y x
train = undefined

-- bfilter?
guess :: Bayes y x -> x -> Bayes y ()
guess = undefined

mle :: Bayes y () -> (y, Double)
mle = undefined

posterior :: h -> (x,y) -> Bayes w (x,y)
posterior = undefined

posterior_predictive :: h -> (x,y) -> x -> 
