module MyPMonad where

import Control.Monad
import Control.Applicative ((<$>))
import Data.List (sort, sortBy, groupBy)

data Dist a = P [(a, Double)] deriving (Show)

instance Functor Dist where
    fmap f (P xs) = P $ map (\(a,p) -> (f a, p)) xs

instance Monad Dist where
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

equally :: Ord a => [a] -> Dist a
equally as =
    let aLength = fromIntegral $ length as
        list = map (\a -> (a, 1 / aLength)) $ sort as
    in
        regroup . P $ list

weighted :: Ord a => [(Double, a)] -> Dist a
weighted as = regroup . P $ map (\(l,r) -> (r,l)) as

probabilityOf :: Ord a => (a -> Bool) -> Dist a -> Double
probabilityOf predicate (P dist) =
    foldr ((+) . snd) 0 $ filter (predicate . fst) dist


pfilter :: (a -> Bool) -> Dist a -> Dist a
pfilter pred (P dist) =
    let xs = filter ( pred . fst ) dist
        totalProbability = foldr (\(_,p) acc -> acc + p) 0 xs
    in
        P $ map (\(x,p) -> (x, p / totalProbability)) xs

regroup :: Eq a => Dist a -> Dist a
regroup (P xs) =
    let addToList (a,pa) [] = [(a,pa)]
        addToList (a,pa) ((b,pb):bs) =
            if a == b
            then (a, pa + pb):bs
            else (b,pb) :(addToList (a,pa) bs)
    in
        P $ foldr addToList [] xs

sortProbabilities :: Dist a -> [(a, Double)]
sortProbabilities (P xs) = sortBy (\l r -> (snd r) `compare` (snd l)) xs
