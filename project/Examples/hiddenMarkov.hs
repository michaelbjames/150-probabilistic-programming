module Example.HiddenMarkov where

import Bayes

data State = Rainy | Sunny
data Action = Walk | Shop | Clean

stateStep :: State -> unused -> Bayes () Action
stateStep Rainy = const $ weightedOberv [(0.1,Walk), (0.4,Shop), (0.5,Clean)])


