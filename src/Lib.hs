module Lib
    (
        calcFuel,
        calcFuelRec,
        findInputForTargetOutput,
        findArgsForTargetOutput
    ) where

import Util
import Intcode

calcFuel :: Int -> Int
calcFuel mass = (div mass 3) - 2

calcFuelRec :: Int -> Int
calcFuelRec mass = 
    let fuel = zeroIfNegative (calcFuel mass)
    in if fuel == 0
        then 0
        else fuel + calcFuelRec(fuel)

findInputForTargetOutput :: Eq b => (a -> b) -> b -> [a] -> Maybe a
findInputForTargetOutput f t = foldl f' Nothing
    where f' a n = case a of
                    Just x -> Just x
                    Nothing -> if f n == t then Just n else Nothing

findArgsForTargetOutput :: Eq c => [a] -> [b] -> (a -> b -> c) -> c -> Maybe (a, b)
findArgsForTargetOutput as bs f t = 
    let f' = uncurry f in
        findInputForTargetOutput f' t (allTuples as bs) 

