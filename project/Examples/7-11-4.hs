module Example.7-11-4 where

import Bayes

newtype Die = Die Int deriving (Show, Eq, Ord)

bag :: Bayes Die Die
bag = undefined

draw3 :: Bayes Die Die -> Bayes [Die] [Die]

-- formerly Dist [(Die, Int)]
roll :: Bayes [Die] [Die] -> Bayes [Die] [Int]

matchCriteria :: ([Int] -> Bool) -> Bayes [Die] [Int] -> Bayes [Die] ()

desiredDieProbability :: ([Die] -> Bool) -> Bayes [Die] () -> Double

