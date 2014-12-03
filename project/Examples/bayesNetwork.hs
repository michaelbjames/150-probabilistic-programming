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
        simpleNode = choose probs
    in
         bind1 (returnLatent parent) (\_ _ -> simpleNode)

family :: Bayes Family Family
family = choose [(0.8,Home), (0.2,Away)]

sick :: Bayes Sick Sick
sick = undefined


light :: Family -> Bayes Family Light
light fam = choosev (lookupError lightTable fam) On Off fam
dog :: (Sick, Family) -> Bayes (Sick, Family) Dog
dog = undefined
bark :: Dog -> Bayes Dog Bark
bark = undefined

network :: Bayes (Sick, Family) (Bark, Light)
network =
    let lightNode :: Bayes Family Light
        lightNode = bindTransform family (\lat -> light lat)
        dogNode :: Bayes (Sick, Family) Dog
        dogNode = liftB2 (\sickFamily _ -> dog sickFamily) sick family
        barkNode :: Bayes Dog Bark
        barkNode = bindTransform dogNode (\lat -> bark lat)
        exitNodes :: Bayes (Dog, Family) (Bark, Light)
        exitNodes = liftB2 spawn barkNode lightNode
    in
        liftB2 (\(sickFamily,_) (_,barkLight) -> spawn sickFamily barkLight) dogNode exitNodes

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
