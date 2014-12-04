module Bayes where


{-
The latent variable is the hidden model on which we will query for a probability
The observable variable are the features that we see and thus can filter on.
-}
data Bayes latent observable = B

{-
Is this a backdoor
-}
spawn :: lat -> obs -> Bayes lat obs
spawn = undefined

returnLatent :: lat -> Bayes lat ()
returnLatent = undefined

returnObserv :: obs -> Bayes () obs
returnObserv = undefined


weightedLatent :: [(Double, lat)] -> Bayes lat ()
weightedLatent = undefined

weightedObserv :: [(Double, obs)] -> Bayes () obs
weightedObserv = undefined


equallyLatent :: [lat] -> Bayes lat ()
equallyLatent = undefined

equallyObserv :: [obs] -> Bayes () obs
equallyObserv = undefined


mapObserv :: (obs -> b) -> Bayes lat obs -> Bayes lat b
mapObserv = undefined

mapLatent :: (lat -> a) -> Bayes lat obs -> Bayes a obs
mapLatent = undefined


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
DiePattern : rollDice
BayesNetwork : choosev
-}
bindObserv :: Bayes lat obs -> (lat -> Bayes z o) -> Bayes lat o
bindObserv = undefined

{- Used:
DiePattern : draw3
-}
bindLatent :: Bayes lat obs -> (lat -> Bayes l z) -> Bayes l obs
bindLatent = undefined

{-
BayesNetwork : network
-}
bindTransform :: Bayes lat obs -> (lat -> obs -> Bayes obs newobs) -> Bayes obs newobs
bindTransform = undefined



{-
DiePattern : rollDice
Notes: It would be better if it were a set of Bayes to a Bayes of sets
-}
bsequence :: [Bayes lat obs] -> Bayes [lat] [obs]
bsequence = undefined

{-
This is a combination of bindObserv and bindLatent
-}
joint :: Bayes lat1 obs1 -> Bayes lat2 obs2 -> Bayes (lat1,lat2) (obs1,obs2)
joint = undefined

merge :: Bayes lat obs -> Bayes lat obs -> Bayes lat obs
merge = undefined


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
