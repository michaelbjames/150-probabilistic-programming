module Examples.DiePatterns where

import Bayes

newtype Die = D Int deriving (Show, Eq, Ord)

-- The division of when to put things on the left or right is not obvious
-- unless you already know the pattern and what you're going to query for.

bag :: Bayes Die ()
bag = undefined

draw3 :: Bayes Die () -> Bayes [Die] ()
draw3 bag =
    bindL bag (\d1 _ ->
    bindL bag (\d2 _ ->
    bindL bag (\d3 _ ->
        returnL [d1, d2, d3] )))

rollDice :: Bayes [Die] () -> Bayes [Die] [Int]
rollDice draw = bindO draw (\die _ -> bsequence (map roll die))

-- formerly Dist [(Die, Int)]
roll :: Die -> Bayes Die Int
roll myDie = bindO (returnL myDie) myRoll
    where
        myRoll :: Die -> a -> Bayes () Int
        myRoll (D 10) _ = equallyO [0..9]
        myRoll (D n) _ = equallyO [1..n]

matchCriteria :: Bayes [Die] [Int] -> Bayes [Die] [Int]
matchCriteria = bfilter criteria
    where
        criteria throws =
            (any (== 11) throws) &&
            (any (== 7) throws) &&
            (any (\x -> x `mod` 4 == 0 && x>0) throws)

desiredDieProbability :: Bayes [Die] [Int] -> Double
desiredDieProbability = probabilityOf (any (== (D 8)))
