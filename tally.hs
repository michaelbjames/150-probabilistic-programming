
type Distribution a = [(a, Double)]

data Die = FairDie Int

regroup :: Distribution a -> Distribution a

support :: Distribution a -> [a]

equally :: [a] -> Distribution a
weighted :: [(a, Double)] -> Distribution a

probabilityOf :: Distribution a -> a -> Double

pmap :: (a -> b) -> Distribution a -> Distribution b
pmap f xs =
    let xs' = map (\(a,b) -> (f a, b)) xs
    in
        regroup xs'

bindx :: Distribution a -> (a -> Distribution b) -> Distribution (a,b)
bindx xs distributionMap =
    let combineDistribution (a,ap) (b,bp) = ((a,b), ap * bp)
        bind' (a, ap) = map (combineDistribution (a, ap)) $ distributionMap a
        allDistributions = map bind' xs 
    in
        foldr (++) [] allDistributions

and :: Distribution a -> Distribution b -> Distribution (a,b)
--given :: Distribution a -> Distribution b -> Distribution (a)

liftp2 :: (a -> b -> c) -> Distribution a -> Distribution b -> Distribution c


die :: Int -> Die
sides :: Die -> Int
dieDistribution :: Die -> Distribution Int

bagDistribution :: Distribution Die

probabilityOf11 =
    let d6 = dieDistribution die 6
        d12 = dieDistribution die 12
        bothDie = bindx d6 (\_ -> d12)
        distribution = pmap (uncurry (+)) bothDie
        isEleven = pmap (==11) distribution
    in
        probabilityOf isEleven True

probabilityOfD6D12 =
    let drawDie n = pmap ((== n) . sides) bagDistribution
        drawD6 = draw 6
        drawD12 = draw 12
        drawBoth = and drawD6 drawD12
    in
        probabilityOf drawBoth (True,True)

