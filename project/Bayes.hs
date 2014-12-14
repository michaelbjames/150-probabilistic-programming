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


weightedL :: [(Double, lat)] -> Bayes lat ()
weightedL = undefined

weightedO :: [(Double, obs)] -> Bayes () obs
weightedO = undefined


equallyL :: [lat] -> Bayes lat ()
equallyL = undefined

equallyO :: [obs] -> Bayes () obs
equallyO = undefined


mapO :: (obs -> b) -> Bayes lat obs -> Bayes lat b
mapO = undefined

mapL :: (lat -> a) -> Bayes lat obs -> Bayes a obs
mapL = undefined


{-
DiePattern : matchCriteria
-}
bfilter :: (obs -> Bool) -> Bayes lat obs -> Bayes lat obs
bfilter = undefined

{-
DiePattern : desiredDieProbability
-}
probabilityOf :: (lat -> Bool) -> Bayes lat obs -> Double
probabilityOf = undefined

{-
this appears to just make a simple distribution from the latent variable
then mix it in.
-}
bindO :: Bayes lat obs -> (lat -> Bayes z o) -> Bayes lat o
bindO = undefined

{- Used:
DiePattern : draw3
-}
bindL :: Bayes lat obs -> (lat -> Bayes l z) -> Bayes l obs
bindL = undefined

{-
BayesNetwork : network
-}
bindC :: Bayes lat obs -> (obs -> Bayes obs newobs) -> Bayes lat newobs
bindC = undefined

{-
DiePattern : rollDice
Notes: It would be better if it were a set of Bayes to a Bayes of sets
-}
bsequence :: [Bayes lat obs] -> Bayes [lat] [obs]
bsequence = undefined

{-
This is a combination of bindO and bindL
-}
joint :: Bayes lat1 obs1 -> Bayes lat2 obs2 -> Bayes (lat1,lat2) (obs1,obs2)
joint = undefined

mergeO :: Bayes lat obs1 -> Bayes lat obs2 -> Bayes lat (obs1,ob2)
mergeO = undefined

expectationMax :: Bayes lat obs -> Bayes prior lat -> Bayes lat obs
expectationMax = undefined

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
