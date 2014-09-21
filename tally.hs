import Data.List (groupBy)

type Distribution a = [(a, Double)]

data Die = FairDie Int

regroup :: Eq a => Distribution a -> Distribution a
regroup xs =
    let tags = groupBy (\(x, px) (y, py) -> x == y) xs
        addProbabilities (_, partial) (tag, total) = (tag, partial + total)
        simplfyTag ts = foldr addProbabilities (head ts) ts
    in
        map simplfyTag tags

--support :: Distribution a -> [a]

equally :: Eq a => [a] -> Distribution a
equally xs =
    let xLength = fromIntegral $ length xs
        dupes = map (\x -> (x, 1 / xLength)) xs
    in
        regroup dupes

--weighted :: [(a, Double)] -> Distribution a

--probabilityOf :: Distribution a -> a -> Double

pmap :: Eq b => (a -> b) -> Distribution a -> Distribution b
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

--liftp2 :: (a -> b -> c) -> Distribution a -> Distribution b -> Distribution c

--drawABfromXwithReplacement :: a -> a -> Distribution a -> double
--drawABfromXwithReplacement a, b, x =
---- The 2 comes from the fact that there are 2 ways, of equal likelihood, to draw an a and a b.
--    (if a == b then 1 else 2) *
--    (probabilityOf a x) * (probabilityOf b x)

--die :: Int -> Die
--sides :: Die -> Int
--dieDistribution :: Die -> Distribution Int

--bagDistribution :: Distribution Die

--probabilityOf11 =
--    let d6 = dieDistribution die 6
--        d12 = dieDistribution die 12
--        bothDie = bindx d6 (\_ -> d12)
--        distribution = pmap (uncurry (+)) bothDie
--        isEleven = pmap (==11) distribution
--    in
--        probabilityOf isEleven True

--probabilityOfD6D12 =
--    let drawDie n = pmap ((== n) . sides) bagDistribution
--        drawD6 = drawDie 6 -- Distribtion Int
--        drawD12 = drawDie 12

--    in
--        probabilityOf drawBoth (True,True)

