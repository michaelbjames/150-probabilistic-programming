
type Distribution a = [(a, Double)]

data Die = FairDie Int

regroup :: Distribution a -> Distribution a

support :: Distribution a -> [a]

equally :: [a] -> Distribution a

probabilityOf :: Distribution a -> a -> Double

pmap :: (a -> b) -> Distribution a -> Distribution b

bindx :: Distribution a -> (a -> Distribution b) -> Distribution (b,a)
bindp :: Distribution a -> (a -> Distribution b) -> Distribution b



die :: Int -> Distribution Die

bag :: Distribution Die

probabilityOf11 =
    let d6 = die 6
        d12 = die 12
        bothDie = bindx d6 (\_ -> d12)
        sumDistribution = pmap (uncurry (+)) bothDie
        isEleven = pmap (==11) sumDistribution
    in
        probabilityOf isEleven True

