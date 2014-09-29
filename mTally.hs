import Control.Monad
import Control.Applicative ((<$>))
import Data.List (sort, sortBy, groupBy)

data Distribution a = P [(a, Double)] deriving (Show)
newtype Die = Die Int deriving (Show, Eq, Ord)
newtype Tally = LR (Integer, Integer) deriving (Show, Eq)

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

sumDistribution :: (Eq a, Num a) => Distribution a -> Distribution a -> Distribution a
sumDistribution xs ys = regroup $ do
    x <- xs
    y <- ys
    return $ x + y

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


--problemF :: Distribution Integer
problemF = regroup $ do
    let d4 = dieToDistribution $ Die 4
    roll <- d4
    rollSum <- regroup $ (+ roll) <$> d4
    let rightTally = if rollSum >= 8 then (1 :: Integer) else (0 :: Integer)
    --return rightTally
    foldr sumDistribution (return 0) (replicate 4 $ return rightTally)



main = do
    answer "B" $ probabilityOf (\x -> x == (12, Die 6, Die 12) || x == (12, Die 12, Die 6)) problemB
    answer "E" $ probabilityOf (\x -> x == (Die 6, Die 12) || x == (Die 12, Die 6)) problemE
    --answer "F" $ probabilityOf (== 3) problemF
    answer "F" $ problemF
    where
        answer number ans = putStrLn $ number ++ ": " ++ show ans
