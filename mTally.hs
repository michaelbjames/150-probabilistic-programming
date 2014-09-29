import Control.Monad
import Control.Applicative ((<$>))
import Data.List (sort, sortBy, groupBy)

data Distribution a = P [(a, Double)] deriving (Show)
newtype Die = Die Int deriving (Show, Eq, Ord)
newtype Tally = LR (Integer, Integer) deriving (Show, Eq)

instance Functor Distribution where
    fmap f (P xs) = P $ map (\(a,p) -> (f a, p)) xs

class Functor pf => PFunctor pf where
    pmap :: Ord b => (a -> b) -> pf a -> pf b

instance PFunctor Distribution where
    pmap f (P xs) = regroup $ map (\(a,p) -> (f a, p)) xs

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

class Monad pm => PMonad pm where
    preturn :: Ord a => a -> pm a
    bindx :: (Ord a, Ord b) => pm a -> (a -> pm b) -> pm (a,b)

instance PMonad Distribution where
    preturn x = P [(x,1.0)]
    bindx (P xs) f =
        let combineDistribution (a,ap) (b,bp) = ((a,b), ap * bp)
            unwrap (P xs) = xs
            bind' (a, ap) = map (combineDistribution (a, ap)) $ unwrap (f a)
            allDistributions = map bind' xs 
        in
            regroup $ foldr (++) [] allDistributions

--------------------------
-- SUPPORTING FUNCTIONS --
--------------------------

equally :: Ord a => [a] -> Distribution a
equally as =
    let aLength = fromIntegral $ length as
        list = map (\a -> (a, 1 / aLength)) $ sort as
    in
        regroup $ list


probabilityOf :: Ord a => (a -> Bool) -> Distribution a -> Double
probabilityOf predicate (P dist) =
    foldr ((+) . snd) 0 $ filter (predicate . fst) dist


pfilter :: (a -> Bool) -> Distribution a -> Distribution a
pfilter pred (P dist) =
    let xs = filter ( pred . fst ) dist
        totalProbability = foldr (\(_,p) acc -> acc + p) 0 xs
    in
        P $ map (\(x,p) -> (x, p / totalProbability)) xs


regroup :: Ord a => [(a,Double)] -> Distribution a
regroup xs =
    let tags = groupBy (\(x, px) (y, py) -> x == y) xs
        addProbabilities (tag, partial) (_, total) = (tag, partial + total)
        simplfyTag ts = foldr addProbabilities (undefined, 0) ts
        sorter = sortBy (\(x,_) (y,_) -> compare x y)
    in
        P . sorter $ map simplfyTag tags


dieToDistribution :: Die -> Distribution Int
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

-------------------
-- QUESTION CODE --
-------------------

problemB :: Distribution (Int, Die, Die)
problemB = do
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
    (\(_,d1,d2) -> (d1,d2)) <$> pfilter (\(sum, d1, d2) -> sum == 12) problemB

problemF :: Distribution Tally
problemF = do
    bag <- bagDistribution
    bothDie <- ((,) bag) <$> bagDistribution
    let sumDie (die1, die2) = do
        d1 <- dieToDistribution die1
        d2 <- dieToDistribution die2
        return (d1 + d2 < 8)
    



main = do
    answer "B" $ probabilityOf (== (12, Die 6, Die 12)) problemB
    answer "E" $ probabilityOf (== (Die 6, Die 12)) problemE
    where
        answer number ans = putStrLn $ number ++ ": " ++ show ans
