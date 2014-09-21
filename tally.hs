import Data.List (groupBy)

type Distribution a = [(a, Double)]
data Die = FairDie Int deriving (Show, Eq)
data Col = Left | Right deriving (Show, Eq)

--data Distribution a = P [(a, Double)] deriving (Show, Eq)


regroup :: Eq a => Distribution a -> Distribution a
regroup xs =
    let tags = groupBy (\(x, px) (y, py) -> x == y) xs
        addProbabilities (tag, partial) (_, total) = (tag, partial + total)
        simplfyTag ts = foldr addProbabilities (undefined, 0) ts
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
probabilityOf elem dist =
    let search = filter (((==) elem) . fst) dist
    in
        case search of
            (_,p) : xs -> p
            _ -> 0

probabilityGiven :: (Eq a, Eq b) => a -> b -> Distribution (a, b) -> Double
probabilityGiven elem given dist =
    let givenDistribution = pfilter (\(_,g) -> g == given) dist
    in
        probabilityOf (elem, given) givenDistribution

pfilter :: (a -> Bool) -> Distribution a -> Distribution a
pfilter pred dist =
    let xs = filter ( pred . fst ) dist
        totalProbability = foldr (\(_,p) acc -> acc + p) 0 xs
    in
        map (\(x,p) -> (x, p / totalProbability)) xs

pmap :: Eq b => (a -> b) -> Distribution a -> Distribution b
pmap f xs =
    let xs' = map (\(a,b) -> (f a, b)) xs
    in
        regroup xs'

bindx :: Distribution a -> (a -> Distribution b) -> Distribution (a,b)
bindx xs f =
    let combineDistribution (a,ap) (b,bp) = ((a,b), ap * bp)
        bind' (a, ap) = map (combineDistribution (a, ap)) $ f a
        allDistributions = map bind' xs 
    in
        foldr (++) [] allDistributions

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

sum11 d1 d2 =
    let dieA = dieDistribution $ die d1
        dieB = dieDistribution $ die d2
        bothDie = bindx dieA (\_ -> dieB)
        distribution = pmap (uncurry (+)) bothDie
    in
        pmap (==11) distribution

d6D12 =
    let drawDie n = pmap ((== n) . sides) bagDistribution
        drawD6 = drawDie 6 -- Distribution Bool
        drawD12 = drawDie 12
    in
        bindx drawD6 (\d -> pmap ((&&) d) drawD12)

d6D12Sum11 = bindx d6D12 (\_ -> sum11 6 12)

sum11ConditionalD6D12 :: Distribution ((Die, Die), Bool)
sum11ConditionalD6D12 =
    let twoDie = bindx bagDistribution (\_ -> bagDistribution) -- Distribution (Die, Die)
    in
        bindx twoDie (\(d1,d2) -> sum11 (sides d1) (sides d2)) -- Distribution ((Die, Die),Bool)


--tallySheet :: (Int -> Bool) -> Distribution ((Die, Die))
--tallySheet


answer :: Show a => String -> a -> IO ()
answer number ans = putStrLn $ number ++ ": " ++ show ans

main :: IO ()
main = do
    answer "B" $ probabilityOf True (sum11 6 12)
    answer "C" $ probabilityOf (True,True) d6D12
    answer "D" $ probabilityOf ((True, True), True) d6D12Sum11
    answer "E" $ probabilityGiven (die 4, die 4) True sum11ConditionalD6D12
