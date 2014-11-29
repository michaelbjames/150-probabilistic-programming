import MyPMonad


pDice = equally [1..6]

newtype Die = Die Int deriving (Show, Eq, Ord)

dieToDistribution :: Die -> Distribution Int
dieToDistribution (Die 10) = equally [0..9]
dieToDistribution (Die n) = equally [1..n]

bagDistribution :: Distribution Die
bagDistribution =
    equally . foldr (++) [] $
        [ replicate 12 $ Die 4
        , replicate 12 $ Die 6
        , replicate 12 $ Die 8
        , replicate 16 $ Die 10
        , replicate 17 $ Die 12
        , replicate 17 $ Die 20
        ]

isSorted :: Ord a => [a] -> Bool
isSorted (l:r:as) = if (l >= r) then isSorted (r:as) else False
isSorted _ = True

drawThree :: Distribution [Die]
drawThree = do
    d1 <- bagDistribution
    d2 <- bagDistribution
    d3 <- bagDistribution
    pfilter isSorted $ return [d1,d2,d3]

rollThree :: Distribution ([Die], [Int])
rollThree = do
    die <- drawThree
    rolls <- pfilter isSorted $ sequence $ map dieToDistribution die
    return (die, rolls)

criteriaMatch :: Distribution ([Die], [Int])
criteriaMatch =
    let criteria :: ([Die],[Int]) -> Bool
        criteria (die, throws) =
            (any (== 11) throws) &&
            (any (== 7) throws) &&
            (any (\x -> x `mod` 4 == 0 && x>0) throws)
    in  pfilter criteria rollThree

probabilityOfD8 :: Double
probabilityOfD8 =
    let predicate (die, throws) =
            any (== (Die 8)) die
    in  probabilityOf predicate criteriaMatch
