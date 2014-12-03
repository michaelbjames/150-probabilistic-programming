module Examples.LinearRegression where

import Bayes

{-
Train
Guess
-}

train :: [(X,Y)] -> Bayes Y X
train = undefined

-- bfilter?
guess :: Bayes Y X -> X -> Bayes Y ()
guess = undefined

mle :: Bayes Y () -> (Y, Double)
mle = undefined
