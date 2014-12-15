import Bayes
import Prelude hiding (lookup)
import Data.Map.Strict (Map, lookup)

data Family = Home | Away deriving (Eq, Ord)
data Light = On | Off deriving (Eq, Ord)
data Dog = In | Out deriving (Eq, Ord)
data Sick = Sick | Healthy deriving (Eq, Ord)
data Bark = Loud | Quiet deriving (Eq, Ord)

choosev :: Ord a => Double -> a -> a -> parent -> Bayes parent a
choosev prob left right parent =
    let probs = [(prob, left), (1 - prob, right)]
        choices = weightedO probs
    in
        bindO (returnL parent) (\_ _ -> choices)

family :: Bayes Family ()
family = weightedL [(0.8,Home), (0.2,Away)]
sick :: Bayes Sick ()
sick = undefined

light :: Family -> a -> Bayes Family Light
light Home _ = choosev 0.95 On Off Home
light Away _ = choosev 0.3 On Off Away
dog :: (Sick, Family) -> a -> Bayes (Sick, Family) Dog
dog _ = undefined
bark :: a -> Dog -> Bayes Dog Bark
bark _ = undefined


network :: Bayes Family (Bark, Light)
network =
    let 
        dogNode :: Bayes (Sick, Family) Dog
        dogNode = bindO (joint sick family) dog
        barkNode :: Bayes (Sick, Family) Bark
        barkNode = bindC dogNode bark
        lightNode :: Bayes (Sick, Family) Light
        lightNode = bindO barkNode (light . snd)
        exitNodes :: Bayes (Sick, Family) (Bark, Light)
        exitNodes = mergeO barkNode lightNode
    in
        bindL exitNodes (\(_,fam) _ -> returnL fam)

-- light : On, Bark : Quiet
observations :: ((Bark, Light) -> Bool) -> Bayes Family (Bark, Light)
observations = flip bfilter network

query :: (Family -> Bool) -> Bayes Family (Bark, Light) -> Double
query = probabilityOf


