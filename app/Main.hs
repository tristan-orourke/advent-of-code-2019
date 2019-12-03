module Main where

import Lib
import Text.Printf
import Text.Show

printAnswer :: Show a => Int -> Int -> a -> IO ()
printAnswer day part answer = do
    let output = (printf "Day %d Part %d: " day part) ++ show answer
    putStrLn output

main :: IO ()
main = do
    input <- intsFromFile "data/day1.txt"
    printAnswer 1 1 (day1Part1 input)
    printAnswer 1 2 (day1Part2 input)

--- >>> main
--- Day 1 Part 1: 3345909
--- Day 1 Part 2: 5015983
---
