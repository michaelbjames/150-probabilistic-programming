import Bayes
import Prelude hiding (lookup)
import Data.Map.Strict (Map, lookup)

data Family = Home | Away deriving (Eq, Ord)
data Light = On | Off deriving (Eq, Ord)
data Dog = In | Out deriving (Eq, Ord)
data Sick = Sick | Healthy deriving (Eq, Ord)
data Bark = Loud | Quiet deriving (Eq, Ord)

choosev :: Double -> a -> a -> parent -> Bayes parent a
choosev prob left right parent =
    let probs = [(prob, left), (1 - prob, right)]
        choices = weightedObserv probs
    in
         bindObserv (returnLatent parent) (\_ -> choices)

family :: Bayes Family ()
family = weightedLatent [(0.8,Home), (0.2,Away)]

sick :: Bayes Sick ()
sick = undefined


light :: Family -> Bayes Family Light
light Home = choosev 0.95 On Off Home
light Away = choosev 0.3 On Off Away
dog :: (Sick, Family) -> Bayes (Sick, Family) Dog
dog = undefined
bark :: Dog -> Bayes Dog Bark
bark = undefined

network :: Bayes (Sick, Family) (Bark, Light)
network =
    let lightNode :: Bayes Family Light
        lightNode = bindObserv family (\lat -> light lat)
        dogNode :: Bayes (Sick, Family) Dog
        dogNode = bindObserv (joint sick family) (\sickFamily -> dog sickFamily)
        barkNode :: Bayes Dog Bark
        barkNode = bindTransform dogNode (\_ obs -> bark obs)
        exitNodes :: Bayes (Dog, Family) (Bark, Light)
        exitNodes = joint barkNode lightNode
    in
        bindObserv dogNode (\_ -> exitNodes)

-- light : On, Bark : Quiet
observations :: ((Bark, Light) -> Bool) -> Bayes (Sick, Family) (Bark, Light)
observations = undefined

query :: ((Sick, Family) -> Bool) -> Bayes (Sick, Family) (Bark, Light)
query = undefined

{-
Notice that the Dog being In or Out is not accessible in the model
-}

lookupError :: Map k v -> k -> v
lookupError = undefined
lightTable :: Map Family Double
lightTable = undefined
