module Examples.DiePatterns where

import Bayes

newtype Die = D Int deriving (Show, Eq, Ord)

-- The division of when to put things on the left or right is not obvious
-- unless you already know the pattern and what you're going to query for.

bag :: Bayes Die ()

draw3 :: Bayes Die () -> Bayes [Die] ()
draw3 bag =
    bindL bag (\d1 ->
    bindL bag (\d2 ->
    bindL bag (\d3 ->
        returnL [d1, d2, d3] )))

-- formerly Dist [(Die, Int)]
roll :: Die -> Bayes Die Int
roll myDie = bindO (returnL myDie) myRoll
    where
        myRoll :: Die -> Bayes () Int
        myRoll (D 10) = equallyO [0..9]
        myRoll (D n) = equallyO [1..n]

rollDice :: Bayes [Die] () -> Bayes [Die] [Int]
rollDice draw = bindO draw (\die -> bsequence (map roll die))

matchCriteria :: Bayes [Die] [Int] -> Bayes [Die] [Int]
matchCriteria = bfilter criteria
    where
        criteria throws =
            (any (== 11) throws) &&
            (any (== 7) throws) &&
            (any (\x -> x `mod` 4 == 0 && x>0) throws)

desiredDieProbability :: Bayes [Die] [Int] -> Double
desiredDieProbability = probabilityOf (any (== (D 8)))
