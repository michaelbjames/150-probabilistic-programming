module Example.BayesNetwork where

import Bayes
import Prelude hiding (lookup)
import Data.Map.Strict (Map, lookup)

data Family = Home | Away deriving (Eq, Ord)
data Light = On | Off deriving (Eq, Ord)
data Dog = In | Out deriving (Eq, Ord)
data Sick = Sick | Healthy deriving (Eq, Ord)
data Bark = Loud | Quiet deriving (Eq, Ord)

lookupError :: k -> Map k v -> v
lookupError = undefined
lightTable :: Map Light Double
lightTable = undefined

choosev :: Double -> a -> a -> parent -> Bayes parent a
choosev prob left right parent =
    let probs = [(prob, left), (1 - prob, right)]
        simpleNode :: Bayes a a
        simpleNode = choose probs left right
    in
        mapl (const parent) simpleNode

family :: Bayes () Family
family = choosev 0.8 Home Away ()
sick :: Bayes () Sick
sick = undefined

light :: Family -> Bayes Family Light
light fam = choosev (lookupError lightTable fam) On Off fam
dog :: (Sick, Family) -> Bayes (Sick, Family) Dog
dog = undefined
bark :: Dog -> Bayes Dog Bark
bark = undefined

network :: Bayes (Sick, Family) (Bark, Light)
network =
    let lightNode = bindGeneral family (\_ obs -> light obs)
        dogNode = liftB2 (\_ sickFamily -> dog sickFamily) sick family
        barkNode = bindGenearl dogNode (\_ obs -> bark obs)

-- light : On, Bark : Quiet
observations :: ((Bark, Light) -> Bool) -> Bayes (Sick, Family) (Bark, Light)
observations = undefined

query :: ((Sick, Family) -> Bool) -> Bayes (Sick, Family) (Bark, Light)
query = undefined

{-
Notice that the Dog being In or Out is not accessible in the model
-}
