import Prelude hiding (Either,Right,Left)
import Data.List (groupBy)

type Distribution a = [(a, Double)]
data Die = FairDie Int deriving (Show, Eq)
data Col = Left | Right deriving (Show, Eq)
type Tally = (Integer, Integer)

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

probabilityOfPredicate :: Eq a => (a -> Bool) -> Distribution a -> Double
probabilityOfPredicate predicate dist = probabilityOf True $ pmap predicate dist

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

bindx :: (Eq a, Eq b) => Distribution a -> (a -> Distribution b) -> Distribution (a,b)
bindx xs f =
    let combineDistribution (a,ap) (b,bp) = ((a,b), ap * bp)
        bind' (a, ap) = map (combineDistribution (a, ap)) $ f a
        allDistributions = map bind' xs 
    in
        regroup $ foldr (++) [] allDistributions

independentBind :: (Eq a, Eq b) => Distribution a -> Distribution b -> Distribution (a,b)
independentBind distA distB = bindx distA (const distB)

independentFold :: Eq a => Distribution a -> Distribution [a] -> Distribution [a]
independentFold a bs = pmap (\(bs',a') -> a':bs') $ bindx bs (const a)


preturn :: Eq a => a -> Distribution a
preturn x = equally [x]

die :: Int -> Die
die = FairDie

sides :: Die -> Int
sides (FairDie n) = n

dieDistribution :: Die -> Distribution Int
dieDistribution (FairDie n) = equally [1..n]

bagDistribution :: Distribution Die
bagDistribution =
    equally . foldr (++) [] $
        [ replicate 12 $ die 4
        , replicate 12 $ die 6
        , replicate 12 $ die 8
        , replicate 16 $ die 10
        , replicate 17 $ die 12
        , replicate 17 $ die 20
        ]


sumDie :: Die -> Die -> (Int -> Bool) -> Distribution Bool
sumDie d1 d2 predicate =
    let bothDie = bindx (dieDistribution d1) (const (dieDistribution d2))
        distribution = pmap (uncurry (+)) bothDie
    in
        pmap predicate distribution

sum11 d1 d2 =
    let dieA = dieDistribution $ die d1
        dieB = dieDistribution $ die d2
        bothDie = bindx dieA (const dieB)
        distribution = pmap (uncurry (+)) bothDie
    in
        pmap (==11) distribution

d6D12 =
    let drawDie n = pmap ((== n) . sides) bagDistribution
        drawD6 = drawDie 6 -- Distribution Bool
        drawD12 = drawDie 12
    in
        bindx drawD6 (\d -> pmap ((&&) d) drawD12)

d6D12Sum11 = bindx d6D12 (const $ sum11 6 12)

sum11ConditionalD6D12 :: Distribution ((Die, Die), Bool)
sum11ConditionalD6D12 =
    let twoDie = bindx bagDistribution (const bagDistribution) -- Distribution (Die, Die)
    in
        bindx twoDie (\(d1,d2) -> sum11 (sides d1) (sides d2)) -- Distribution ((Die, Die),Bool)


tallySheet :: (Int -> Bool) -> Distribution ((Die, Die), Tally)
tallySheet predicate =
    let -- Distribution (Die, Die)
        twoDie = bindx bagDistribution (const bagDistribution)
        -- mapToColumn :: Bool -> Col
        mapToColumn b = if b == True then Left else Right
        -- throw :: (Die, Die) -> Distribution Col
        throw (d1, d2) = pmap mapToColumn $ sumDie d1 d2 predicate
        -- baseThrowCols :: (Die, Die) -> Distribution [Col]
        baseThrowCols dice = pmap (\x -> [x]) $ throw dice
        -- throwMany :: (Die, Die) -> Distribution [Col]
        throwMany dice =
            pmap (\(_,cols) -> cols) $
            bindx (preturn [1..3])
                (\(_:xs) -> foldr (\base acc -> independentFold (throw dice) acc) (baseThrowCols dice) xs)
        -- tally :: [Col] -> Tally
        tally cols =
            foldr (\base (l, r) ->
                    case base of
                        Left -> (l+1, r)
                        Right -> (l, r+1))
                (0, 0)
                cols
        -- orderIndependent :: (Die, Die) -> Distribution Tally
        orderIndependent dice = pmap tally $ throwMany dice
        -- thirtyThrows :: Distribution ((Die,Die),[Col])
        thirtyThrows = bindx twoDie orderIndependent
    in
        thirtyThrows




main :: IO ()
main = do
    answer "B" $ probabilityOf True (sum11 6 12)
    answer "C" $ probabilityOf (True,True) d6D12
    answer "D" $ probabilityOf ((True, True), True) d6D12Sum11
    answer "E" $ probabilityGiven (die 4, die 4) True sum11ConditionalD6D12
    answer "F" $ probabilityGiven (die 4, die 4) (0,3) (tallySheet (< 8))
    --answer "rawF" $ tallySheet (< 8)

    where
        answer number ans = putStrLn $ number ++ ": " ++ show ans
