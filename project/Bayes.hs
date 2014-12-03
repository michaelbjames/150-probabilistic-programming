module Bayes where


{-
The latent variable is the hidden model on which we will query for a probability
The observable variable are the features that we see and thus can filter on.
-}
data Bayes latent observable = B

spawn :: latent -> observable -> Bayes latent observable
spawn = undefined


{-

-}
returnLatent :: latent -> Bayes latent ()
returnLatent = undefined

{-
Good for Bayes Nets
-}
choose :: [(Double, lat)] -> Bayes lat lat
choose = undefined

{-
Helpful for Dice-world problems
-}
equally :: [lat] -> Bayes lat lat
equally = undefined


mapr :: (obs -> b) -> Bayes latent observable -> Bayes latent b
mapr = undefined

mapl :: (lat -> b) -> Bayes latent observable -> Bayes b observable
mapl = undefined

doubleMap :: (lat -> a) -> (obs -> b) -> Bayes lat obs -> Bayes a b
doubleMap = undefined

{-
DiePattern : matchCriteria
-}
bfilter :: (obs -> Bool) -> Bayes lat obs -> Bayes lat ()
bfilter = undefined

{-
DiePattern : desiredDieProbability
-}
probabilityOf :: (lat -> Bool) -> Bayes lat obs -> Double
probabilityOf = undefined

join1 :: Bayes lat (Bayes x y) -> Bayes (lat,x) y
join1 = undefined

join2 :: Bayes (Bayes prior latent) obs -> Bayes (prior, latent) obs
join2 = undefined

{-
DiePattern : rollDice
-}
bind1 :: Bayes lat obs -> (lat -> obs -> Bayes unused o) -> Bayes lat o
bind1 = undefined

{- Used:
DiePattern : draw3
-}
bindLatent :: Bayes lat obs -> (lat -> Bayes l2 o2) -> Bayes l2 o2
bindLatent = undefined

{-
BayesNetwork : network
-}
bindTransform :: Bayes lat obs -> (obs -> Bayes obs newobs) -> Bayes obs newobs
bindTransform = undefined

-- this is like M0 a -> (b -> a -> M1 c) -> M1 c
-- It's a monad transformer...
-- Would you ever want to change the latent variable?
bindGeneral :: Bayes lat obs -> (lat -> obs -> Bayes a b) -> Bayes a b
bindGeneral = undefined

liftB2 :: ((lat1,lat2) -> (obs1,obs2) -> Bayes a b) ->
         Bayes lat1 obs1 ->
         Bayes lat2 obs2 ->
         Bayes a b
liftB2 = undefined

{-
DiePattern : roll
-}
transform :: Bayes lat1 obs -> (lat1 -> obs -> lat2) -> Bayes lat2 obs
transform = undefined

{-
DiePattern : rollDice
Notes: It would be better if it were a set of Bayes to a Bayes of sets
-}
bsequence :: [Bayes lat obs] -> Bayes [lat] [obs]
bsequence = undefined

