module Main where

import Util
import Lib
import Intcode
import Wires
import Passwords
import Orbits
import Text.Printf
import Text.Show

printAnswer :: Show a => Int -> Int -> a -> IO ()
printAnswer day part answer = do
    let output = (printf "Day %d Part %d: " day part) ++ show answer
    putStrLn output

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