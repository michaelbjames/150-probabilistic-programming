module Bayes where

import qualified MyPMonad as MPM

{-
The latent variable is the hidden model on which we will query for a probability
The observable variable are the features that we see and thus can filter on.
-}
type Bayes latent observable = MPM.Dist (latent, observable)

{-
Is this a backdoor
-}
spawn :: lat -> obs -> Bayes lat obs
spawn l o = return (l,o)

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


{-
DiePattern : matchCriteria
-}
bfilter :: (obs -> Bool) -> Bayes lat obs -> Bayes lat obs
bfilter pred = MPM.pfilter (\(_,obs) -> pred obs)

{-
DiePattern : desiredDieProbability
-}
probabilityOf :: (Ord lat, Ord obs) => (lat -> Bool) -> Bayes lat obs -> Double
probabilityOf pred = MPM.probabilityOf (\(lat,_) -> pred lat)

{-
this appears to just make a simple distribution from the latent variable
then mix it in.
-}
bindO :: Bayes lat obs -> (lat -> obs -> Bayes z o) -> Bayes lat o
bindO b f = b >>= (\(lat, obs) -> fmap (\(l,o) -> (lat,o)) (f lat obs))

{- Used:
DiePattern : draw3
-}
bindL :: Bayes lat obs -> (lat -> obs -> Bayes l z) -> Bayes l obs
bindL b f = b >>= (\(lat, obs) -> fmap (\(l,o) -> (l,obs)) (f lat obs))

{-
BayesNetwork : network
-}
bindC :: Bayes lat obs -> (lat -> obs -> Bayes obs newobs) -> Bayes lat newobs
bindC b f = b >>= (\(lat, obs) -> fmap (\(o,newo) -> (lat,newo)) (f lat obs))

{-
DiePattern : rollDice
Notes: It would be better if it were a set of Bayes to a Bayes of sets
-}
bsequence :: [Bayes lat obs] -> Bayes [lat] [obs]
bsequence bs =
    let
        listPairs = sequence bs
        accumulator :: [(lat, obs)] -> ([lat],[obs])
        accumulator = foldr (\(l,o) (ls,os) -> (l:ls, o:os)) ([],[])
    in
        fmap accumulator listPairs

{-
This is a combination of bindO and bindL
-}
joint :: Bayes lat1 obs1 -> Bayes lat2 obs2 -> Bayes (lat1,lat2) (obs1,obs2)
joint = undefined

mergeO :: Bayes lat obs1 -> Bayes lat obs2 -> Bayes lat (obs1,ob2)
mergeO = undefined

------------------------------------------------------
---------------------- REJECTED ----------------------
------------------------------------------------------

{-
Helpful for Dice-world problems
This seems like another back door though.
-}
equally :: [a] -> Bayes a a
equally = undefined

{-
Good for Bayes Nets
-}
weighted :: [(Double, a)] -> Bayes a a
weighted = undefined

doubleMap :: (lat -> a) -> (obs -> b) -> Bayes lat obs -> Bayes a b
doubleMap = undefined

-- this is like M0 a -> (b -> a -> M1 c) -> M1 c
-- It's a monad transformer...
-- Would you ever want to change the latent variable?
bindGeneral :: Bayes lat obs -> (lat -> obs -> Bayes a b) -> Bayes a b
bindGeneral = undefined

join1 :: Bayes lat (Bayes x y) -> Bayes (lat,x) y
join1 = undefined

join2 :: Bayes (Bayes prior latent) obs -> Bayes (prior, latent) obs
join2 = undefined

{-
DiePattern : roll
-}
transform :: Bayes lat1 obs -> (lat1 -> obs -> lat2) -> Bayes lat2 obs
transform = undefined

{-
feels like a backdoor
BayesNetwork : network
-}
bind2General :: ((lat1,lat2) -> (obs1,obs2) -> Bayes a b) ->
         Bayes lat1 obs1 ->
         Bayes lat2 obs2 ->
         Bayes a b
bind2General = undefined
