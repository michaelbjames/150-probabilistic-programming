module Example.HiddenMarkov where

import Bayes

data State = Rainy | Sunny deriving (Eq, Ord)
data Action = Walk | Shop | Clean deriving (Eq, Ord)

choosev :: (Ord parent, Ord a) => Double -> a -> a -> parent -> Bayes parent a
choosev prob left right parent =
    let probs = [(prob, left), (1 - prob, right)]
        choices = weightedO probs
    in
        bindO (returnL parent) (\_ _ -> choices)

stateAction :: State -> Bayes () Action
stateAction Rainy = weightedO [(0.1,Walk), (0.4,Shop), (0.5,Clean)]
stateAction Sunny = weightedO [(0.6,Walk), (0.3,Shop), (0.1,Clean)]

stateStep :: State -> Bayes State ()
stateStep Rainy = weightedL [(0.7,Rainy), (0.3, Sunny)]
stateStep Sunny = weightedL [(0.4,Rainy), (0.6, Sunny)]

stateAggregate :: State -> Bayes State Action
stateAggregate st =
    let actioned = stateAction st
    in
        bindL actioned (\_ _ -> stateStep st)

startState :: Bayes [State] ()
startState = weightedL [(0.6,[Rainy]), (0.4, [Sunny])]

stepN :: Int -> [State] -> () -> Bayes [State] ()
stepN 0 (l:ls) _ = stateStep l `bindL` (\l' _ -> returnL (l':l:ls))
stepN n (l:ls) _ = stateStep l `bindL` (\l' -> stepN (n-1) (l':l:ls))

actionStates :: Bayes [State] () -> Bayes [State] [Action]
actionStates states = states `bindO` (\ls _ -> bsequence (map stateAction ls))

nStates :: Int -> Bayes [State] [Action]
nStates n =
    let statesStepped = startState `bindL` (stepN n)
    in
        actionStates statesStepped


markov :: Bayes [State] [Action]
markov = nStates 5

conditioned :: Bayes [State] [Action]
conditioned =
    let myCondition actions = length (filter (==Clean) actions) >= 2
    in
        bfilter myCondition markov

queried :: Double
queried =
    let myQuery states = length (filter (==Sunny) states) >= 3
    in
        probabilityOf myQuery conditioned

