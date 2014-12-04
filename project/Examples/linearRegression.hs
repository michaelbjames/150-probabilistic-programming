module Examples.LinearRegression where

import Bayes

{-
Train
Guess
-}

train :: [(x,y)] -> Bayes (Bayes w y) x
train = (foldr1 merge) . (map (uncurry $ flip spawn))

{-
Distribution over ys where x is held constant at the given value.
-}
guess :: Eq x => x -> Bayes y x -> Bayes y x
guess datum = bfilter (==datum)

mle :: Bayes y () -> (y, Double)
mle = undefined
