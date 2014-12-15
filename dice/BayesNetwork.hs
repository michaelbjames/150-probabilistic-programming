import MyPMonad

data Family = Home | Away deriving (Eq, Ord)
data Light = On | Off deriving (Eq, Ord)
data Dog = In | Out deriving (Eq, Ord)
data Sick = Sick | Healthy deriving (Eq, Ord)
data Bark = Loud | Quiet deriving (Eq, Ord)

choosev :: Double -> a -> a -> P a
choosev p l r = undefined

family :: Dist Family
family = choosev 0.8 Home Away

sick :: Dist Sick
sick = choosev 0.03 Sick Healthy

light :: Family -> Dist Light
light Home = choosev 0.95 On Off
light Away = choosev 0.4 On Off

dog :: (Sick, Family) -> Dist Dog
dog = undefined

bark :: Dog -> Dist Bark
bark = undefined

network :: Dist (Family, Light, Sick, Bark)
network = do
    f <- family
    s <- sick
    l <- light f
    d <- dog (s,f)
    b <- bark d
    return (f,l,s,b)

observations :: ((Family, Light, Sick, Bark) -> Bool) ->
                Dist (Family, Light, Sick, Bark) ->
                Dist (Family, Light, Sick, Bark)
observations pred = pfilter pred network

query :: ((Family, Light, Sick, Bark) -> Bool) -> Dist (Family, Light, Sick, Bark)
query pred = probabilityOf pred observations


