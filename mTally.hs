import Control.Monad
import Control.Applicative ((<$>))
import Data.List (sort, sortBy, groupBy)

data Distribution a = P [(a, Double)] deriving (Show)
newtype Die = Die Int deriving (Show, Eq, Ord)

instance Functor Distribution where
    fmap f (P xs) = P $ map (\(a,p) -> (f a, p)) xs

instance Monad Distribution where
    return x = P [(x, 1.0)]
    (P xs) >>= f =
        let combineDistribution (a,ap) (b,bp) = (b, ap * bp)
            bind' (a, ap) =
                let (P news) = f a
                in
                    map (combineDistribution (a, ap)) news
            allDistributions = map bind' xs 
        in
            P $ foldr (++) [] allDistributions

--------------------------
-- POTENTIAL EXTENSIONS --
--------------------------

class Functor f => FunctorP f where
    fmap :: Eq b => (a -> b) -> f a -> f b

instance FunctorP Distribution where
    fmap f ps = regroup $ f <$> ps

class Monad m => MonadP m where
    returnp :: Eq a => a -> m a
    bindx :: Eq b => m a -> (a -> m b) -> m b

instance MonadP Distribution where
    returnp x = P [(x, 1.0)]
    bindx ps f = regroup $ ps >>= f

--------------------------
-- SUPPORTING FUNCTIONS --
--------------------------

equally :: Ord a => [a] -> Distribution a
equally as =
    let aLength = fromIntegral $ length as
        list = map (\a -> (a, 1 / aLength)) $ sort as
    in
        regroup . P $ list

probabilityOf :: Ord a => (a -> Bool) -> Distribution a -> Double
probabilityOf predicate (P dist) =
    foldr ((+) . snd) 0 $ filter (predicate . fst) dist


pfilter :: (a -> Bool) -> Distribution a -> Distribution a
pfilter pred (P dist) =
    let xs = filter ( pred . fst ) dist
        totalProbability = foldr (\(_,p) acc -> acc + p) 0 xs
    in
        P $ map (\(x,p) -> (x, p / totalProbability)) xs

regroup :: Eq a => Distribution a -> Distribution a
regroup (P xs) =
    let addToList (a,pa) [] = [(a,pa)]
        addToList (a,pa) ((b,pb):bs) =
            if a == b
            then (a, pa + pb):bs
            else (b,pb) :(addToList (a,pa) bs)
    in
        P $ foldr addToList [] xs

sortProbabilities :: Distribution a -> [(a, Double)]
sortProbabilities (P xs) = sortBy (\l r -> (snd r) `compare` (snd l)) xs

dieToDistribution :: Die -> Distribution Int
dieToDistribution (Die 10) = equally [0..9]
dieToDistribution (Die n) = equally [1..n]

bagDistribution :: Distribution Die
bagDistribution =
    equally . foldr (++) [] $
        [ replicate 12 $ Die 4
        , replicate 12 $ Die 6
        , replicate 12 $ Die 8
        , replicate 16 $ Die 10
        , replicate 17 $ Die 12
        , replicate 17 $ Die 20
        ]

sumDistribution :: (Eq a, Num a) => Distribution a -> Distribution a -> Distribution a
sumDistribution xs ys = regroup $ do
    x <- xs
    y <- ys
    return $ x + y

-------------------
-- QUESTION CODE --
-------------------

problemD :: Distribution (Int, Die, Die)
problemD = do
    bag <- bagDistribution
    bothDie <- ((,) bag) <$> bagDistribution
    let sumDie (die1, die2) = do
        d1 <- dieToDistribution die1
        d2 <- dieToDistribution die2
        return ((d1 + d2), die1, die2)
    sum <- sumDie bothDie
    return sum

problemE :: Distribution (Die, Die)
problemE =
    (\(_,d1,d2) -> (d1,d2)) <$> pfilter (\(sum, d1, d2) -> sum == 12) problemD


problemF :: Distribution Int
problemF = regroup $ do
    let d4 = dieToDistribution $ Die 4
    roll <- d4
    let rollSum = (+ roll) <$> d4
    let rightTally = (\x -> if x >= 8 then 1 else 0) <$> rollSum
    foldr sumDistribution (return 0) (replicate 30 rightTally)

-- The distribution of getting N tally marks on the right side from picking any
-- pair of dice and splitting on (sum >= 8).
--totalTallies :: Distribution Int
totalTallies = regroup $ do
    die1 <- bagDistribution
    die2 <- bagDistribution
    let roll1 = dieToDistribution die1
    let roll2 = dieToDistribution die2
    let rollSum = sumDistribution roll1 roll2
    let rightTally = (\x -> if x >= 8 then 1 else 0) <$> rollSum
    rightMarks <- foldr sumDistribution (return 0) (replicate 30 rightTally)
    return (die1, die2, rightMarks)

problemH = totalTallies

dieForTally n =
    let nMarks = pfilter (\(d1, d2, marks) -> marks == n) totalTallies
    in
        (\(d1,d2,_) -> (d1,d2)) <$> nMarks

main = do
    answer "D" $ probabilityOf (\x -> x == (12, Die 6, Die 12) || x == (12, Die 12, Die 6)) problemD
    answer "E" $ probabilityOf (\x -> x == (Die 6, Die 12) || x == (Die 12, Die 6)) problemE
    --answer "F" $ probabilityOf (== 3) problemF
    answer "F2" $ probabilityOf (\x -> x == (Die 4, Die 4, 3)) totalTallies
    --answer "Two D4s given 3 right marks" $ probabilityOf (== (Die 4, Die 4)) (dieForTally 3)
    answer "The Odds" $ sortProbabilities (dieForTally 3)
    --answer "F" $ problemF
    where
        answer number ans = putStrLn $ number ++ ": " ++ show ans
