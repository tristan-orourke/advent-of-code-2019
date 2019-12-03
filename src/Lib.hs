module Lib
    (
        calcFuel,
        calcFuelRec,
        readPos,
        intcodes1
    ) where

import Util

calcFuel :: Int -> Int
calcFuel mass = (div mass 3) - 2

calcFuelRec :: Int -> Int
calcFuelRec mass = 
    let fuel = zeroIfNegative (calcFuel mass)
    in if fuel == 0
        then 0
        else fuel + calcFuelRec(fuel)

readPos :: [Int] -> Int -> Int
readPos l pos = l !! (l !! pos)

opcodeFunc :: (Int -> Int -> Int) -> [Int] -> Int -> [Int]
opcodeFunc f l pos = 
    let (a, b) = (readPos l (pos+1), readPos l (pos + 2))
    in replaceAt l (l !! (pos+3)) (f a b) 

opcodeAdd :: [Int] -> Int -> [Int]
opcodeAdd = opcodeFunc (+)

opcodeMult :: [Int] -> Int -> [Int]
opcodeMult = opcodeFunc (*)

intcodesRec :: [Int] -> Int -> [Int]
intcodesRec program pos = 
    let code = program !! pos
    in case code of
        1 -> intcodesRec (opcodeAdd program pos) (pos + 4)
        2 -> intcodesRec (opcodeMult program pos) (pos + 4)
        99 -> program
        _ -> program

intcodes1 :: [Int] -> [Int]
intcodes1 program = intcodesRec program 0


