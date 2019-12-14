module Main where

import Util
import Lib
import Intcode
import Wires
import Passwords
import Orbits
import Text.Printf
import Text.Show
import Amplifiers
import Image

printAnswer :: Show a => Int -> Int -> a -> IO ()
printAnswer day part answer = do
    let output = (printf "Day %d Part %d: " day part) ++ show answer
    putStrLn output

drawAnswer :: Int -> Int -> String -> IO ()
drawAnswer day part answer = do
    let output = (printf "Day %d Part %d:\n" day part) ++ answer
    putStr output

day1Part1 :: [Int] -> Int
day1Part1 = sum . map calcFuel

day1Part2 :: [Int] -> Int
day1Part2 = sum . map calcFuelRec

day2Part1 :: [Int] -> Int
day2Part1 = head . (runWithNounVerb 12 2)
day2Part2 :: [Int] -> Int -> Int
day2Part2 m t = 
    let transformNounVerb (n, v) = 100 * n + v in
        let f n v = head (runWithNounVerb n v m) in
            maybe 0 transformNounVerb (findArgsForTargetOutput [0..99] [0..99] f t)

day3Part1 :: [String] -> Int
day3Part1 xs = smallestIntersectDistOfPaths (xs !! 0) (xs !! 1)
day3Part2 :: [String] -> Int
day3Part2 xs = lowestIntersectionLengthOfPaths (xs !! 0) (xs !! 1)

day4Part1 :: Int -> Int -> Int
day4Part1 min max = countValidInRange min max
day4Part2 :: Int -> Int -> Int
day4Part2 min max = countValidStrictInRange min max

day5Part1 :: [Int] -> Int
day5Part1 = outputOfRunProgram 1
day5Part2 :: [Int] -> Int
day5Part2 = outputOfRunProgram 5

day6Part1:: [String] -> Int
day6Part1 = totalDirectIndirectOrbits
day6Part2 :: [String] -> Int
day6Part2 = countOrbitJumps "YOU" "SAN" 

day7Part1 :: [Int] -> Int
day7Part1 = maxFiveAmplifiersOutput
day7Part2 :: [Int] -> Int
day7Part2 = maxBasicLoop

day8Part1 :: [Int] -> Int
day8Part1 xs = 
    let l = layerWithFewestX 0 (digitsToLayers 25 6 xs) in
        countIn 1 l * countIn 2 l
day8Part2 :: [Int] -> String
day8Part2 xs = drawPixels 25 $ renderLayers $ digitsToLayers 25 6 xs

main :: IO ()
main = do
    input1 <- intLinesFromFile "data/day1.txt"
    printAnswer 1 1 (day1Part1 input1)
    printAnswer 1 2 (day1Part2 input1)
    input2 <- intRowFromFile "data/day2.txt"
    printAnswer 2 1 (day2Part1 input2)
    printAnswer 2 2 (day2Part2 input2 19690720)
    input3 <- strLinesFromFile "data/day3.txt"
    printAnswer 3 1 (day3Part1 input3)
    printAnswer 3 2 (day3Part2 input3)
    printAnswer 4 1 (day4Part1  158126 624574)
    printAnswer 4 2 (day4Part2  158126 624574)
    input5 <- intRowFromFile "data/day5.txt"
    printAnswer 5 1 (day5Part1 input5)
    printAnswer 5 2 (day5Part2 input5)
    input6 <- strLinesFromFile "data/day6.txt"
    printAnswer 6 1 (day6Part1 input6)
    printAnswer 6 2 (day6Part2 input6)
    input7 <- intRowFromFile "data/day7.txt"
    printAnswer 7 1 (day7Part1 input7)
    printAnswer 7 2 (day7Part2 input7)
    input8 <- digitsFromFile "data/day8.txt"
    printAnswer 8 1 (day8Part1 input8)
    drawAnswer 8 2 (day8Part2 input8)