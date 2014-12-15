module Bayes where

import qualified MyPMonad as MPM

{-
The latent variable is the hidden model on which we will query for a probability
The observable variable are the features that we see and thus can filter on.
-}
type Bayes latent observable = MPM.Dist (latent, observable)

returnL :: lat -> Bayes lat ()
returnL l = return (l,())

returnO :: obs -> Bayes () obs
returnO o = return ((),o)


weightedL :: Ord lat => [(Double, lat)] -> Bayes lat ()
weightedL xs = fmap (\x -> (x,())) (MPM.weighted xs)

weightedO :: Ord obs => [(Double, obs)] -> Bayes () obs
weightedO xs = fmap (\x -> ((),x)) (MPM.weighted xs)


equallyL :: Ord lat => [lat] -> Bayes lat ()
equallyL ls = fmap (\x -> (x,())) (MPM.equally ls)

equallyO :: Ord obs => [obs] -> Bayes () obs
equallyO os = fmap (\x -> ((),x)) (MPM.equally os)


bfilter :: (obs -> Bool) -> Bayes lat obs -> Bayes lat obs
bfilter pred = MPM.pfilter (\(_,obs) -> pred obs)


probabilityOf :: (Ord lat, Ord obs) => (lat -> Bool) -> Bayes lat obs -> Double
probabilityOf pred = MPM.probabilityOf (\(lat,_) -> pred lat)


bindO :: Bayes lat obs -> (lat -> obs -> Bayes z o) -> Bayes lat o
bindO b f = b >>= (\(lat, obs) -> fmap (\(l,o) -> (lat,o)) (f lat obs))


bindL :: Bayes lat obs -> (lat -> obs -> Bayes l z) -> Bayes l obs
bindL b f = b >>= (\(lat, obs) -> fmap (\(l,o) -> (l,obs)) (f lat obs))


bindC :: Bayes lat obs -> (lat -> obs -> Bayes obs newobs) -> Bayes lat newobs
bindC b f = b >>= (\(lat, obs) -> fmap (\(o,newo) -> (lat,newo)) (f lat obs))


bsequence :: [Bayes lat obs] -> Bayes [lat] [obs]
bsequence bs =
    let
        listPairs = sequence bs
        accumulator :: [(lat, obs)] -> ([lat],[obs])
        accumulator = foldr (\(l,o) (ls,os) -> (l:ls, o:os)) ([],[])
    in
        fmap accumulator listPairs


joint :: Bayes lat1 obs1 -> Bayes lat2 obs2 -> Bayes (lat1,lat2) (obs1,obs2)
joint = undefined

mergeO :: Bayes lat obs1 -> Bayes lat obs2 -> Bayes lat (obs1,ob2)
mergeO = undefined

