module Bayes where


{-
The latent variable is the hidden model on which we will query for a probability
The observable variable are the features that we see and thus can filter on.
-}
data Bayes latent observable = B latent observable

spawn :: latent -> observable -> Bayes latent observable
spawn = undefined

flat :: latent -> Bayes latent latent
flat = undefined

choose :: [(Double, lat)] -> Bayes lat lat
choose = undefined

-- Could lead to zero probabilities?
-- uninteresting queries?
spawn2 :: observable -> Bayes observable observable
spawn2 = undefined

mapr :: (obs -> b) -> Bayes latent observable -> Bayes latent b
mapr = undefined

mapl :: (lat -> b) -> Bayes latent observable -> Bayes b observable
mapl = undefined

doubleMap :: (lat -> a) -> (obs -> b) -> Bayes lat obs -> Bayes a b
doubleMap = undefined

-- but maybe it should be this
bfilter :: (obs -> Bool) -> Bayes lat obs -> Bayes lat obs
bfilter = undefined

probabilityOf :: (lat -> Bool) -> Bayes lat obs -> Double
probabilityOf = undefined

join1 :: Bayes lat (Bayes x y) -> Bayes (lat,x) y
join1 = undefined

join2 :: Bayes (Bayes prior latent) obs -> Bayes (prior, latent) obs
join2 = undefined

bind1 :: Bayes lat obs -> (lat -> obs -> Bayes () o) -> Bayes lat o
bind1 = undefined

-- this is like M0 a -> (b -> a -> M1 c) -> M1 c
-- It's a monad transformer...
-- Would you ever want to change the latent variable?
bindGeneral :: Bayes lat obs -> (lat -> obs -> Bayes a b) -> Bayes a b
bindGeneral = undefined

liftB2 :: ((lat1,lat2) -> (obs1,obs2) -> Bayes a b) ->
         Bayes lat1 obs1 ->
         Bayes lat2 obs2 ->
         Bayes a b

transform :: Bayes lat1 obs -> (lat1 -> obs -> lat2) -> Bayes lat2 ()
transform = undefined

