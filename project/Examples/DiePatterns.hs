module Examples.DiePatterns where

import Bayes

newtype Die = D Int deriving (Show, Eq, Ord)

-- The division of when to put things on the left or right is not obvious
-- unless you already know the pattern and what you're going to query for.

bag :: Bayes Die ()
bag = undefined

draw3 :: Bayes Die () -> Bayes [Die] ()
draw3 bag =
    bindLatent bag (\d1 ->
    bindLatent bag (\d2 ->
    bindLatent bag (\d3 ->
        returnLatent [d1, d2, d3] )))

-- formerly Dist [(Die, Int)]
roll :: Die -> Bayes Die Int
roll myDie = bindObserv (returnLatent myDie) myRoll
    where
        myRoll :: Die -> Bayes () Int
        myRoll (D 10) = equallyObserv [0..9]
        myRoll (D n) = equallyObserv [1..n]

rollDice :: Bayes [Die] () -> Bayes [Die] [Int]
rollDice draw = bindObserv draw (\die -> bsequence (map roll die))

matchCriteria :: ([Int] -> Bool) -> Bayes [Die] [Int] -> Bayes [Die] [Int]
matchCriteria = bfilter

desiredDieProbability :: ([Die] -> Bool) -> Bayes [Die] [Int] -> Double
desiredDieProbability = probabilityOf
