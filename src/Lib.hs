module Lib
    ( 
        strToInt,
        intsFromFile,
        calcFuel,
        calcFuelRec,
        day1Part1,
        day1Part2,
        intcode1
    ) where

strToInt :: String -> Int
strToInt = read

intsFromFile :: String -> IO [Int]
intsFromFile file = do
    input <- readFile file
    let numInput = map strToInt (lines input)
    return numInput

zeroIfNegative :: Int -> Int
zeroIfNegative = max 0

calcFuel :: Int -> Int
calcFuel mass = (div mass 3) - 2

calcFuelRec :: Int -> Int
calcFuelRec mass = 
    let fuel = zeroIfNegative (calcFuel mass)
    in if fuel == 0
        then 0
        else fuel + calcFuelRec(fuel)

day1Part1 :: [Int] -> Int
day1Part1 = sum . map calcFuel

day1Part2 :: [Int] -> Int
day1Part2 = sum . map calcFuelRec

intcode1 :: [Int] -> [Int]
intcode1 program = program


