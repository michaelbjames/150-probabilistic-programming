import Data.List (groupBy)

type Distribution a = [(a, Double)]

data Die = FairDie Int
instance Eq Die where
    (FairDie a) == (FairDie b) = a == b

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

probabilityOf :: Eq a => a -> Distribution a -> Double
probabilityOf elem dist = snd $ head $ filter (((==) elem) . fst) dist

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

-- The 2 comes from the fact that there are 2 ways, of equal likelihood, to draw an a and a b.
drawABfromXwithReplacement :: Eq a => a -> a -> Distribution a -> Double
drawABfromXwithReplacement a b x =
    (if a == b then 1 else 2) * (probabilityOf a x) * (probabilityOf b x)

die :: Int -> Die
die = FairDie

sides :: Die -> Int
sides (FairDie n) = n

dieDistribution :: Die -> Distribution Int
dieDistribution (FairDie n) = equally [1..n]

bagDistribution :: Distribution Die
bagDistribution =
    let d4 = die 4
        d6 = die 6
        d10 = die 10
        d12 = die 12
        d20 = die 20
    in
        equally
            [ d4
            , d6
            , d10
            , d12
            , d20
            ]

probabilityOf11 =
    let d6 = dieDistribution $ die 6
        d12 = dieDistribution $ die 12
        bothDie = bindx d6 (\_ -> d12)
        distribution = pmap (uncurry (+)) bothDie
        isEleven = pmap (==11) distribution
    in
        probabilityOf True isEleven

probabilityOfD6D12 =
    let drawDie n = pmap ((== n) . sides) bagDistribution
        drawD6 = drawDie 6 -- Distribution Bool
        drawD12 = drawDie 12
        drawBoth = bindx drawD6 (\d -> pmap ((&&) d) drawD12)
    in
        probabilityOf (True,True) drawBoth

answer :: Show a => String -> a -> IO ()
answer number ans = putStrLn $ number ++ ": " ++ show ans

main :: IO ()
main = do
    answer "B" probabilityOf11
    answer "C1" probabilityOfD6D12
