bagDistribution :: Distribution Die

problem1 =
    let allDie :: Distribution [Die]
        allDie =
            liftM3 (\d1 d2 d3 -> [d1:d2:d3])
                bagDistribution bagDistribution bagDistribution
        throws :: Distribution [(Die,Int)]
        throws = allDie >>= map (\d -> (d, dieToDistribution d))
        matcher :: (Die, Int) -> Bool
        matcher (die, face) =
            face == 7 || face == 11 || face `mod` 4 == 0
    in
        pfilter (\throw ->
            (all matcher throw) throws



