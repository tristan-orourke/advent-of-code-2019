module Util
    ( 
        strToInt,
        splitOn,
        intLinesFromFile,
        intRowFromFile,
        strLinesFromFile,
        zeroIfNegative,
        replaceAt,
        replaceIn,
        isBetween,
        allTwoArgOutputs,
        allTuples,
        leftToMaybe,
        maybeToLeft,
        addMaybe
    ) where

import Data.Maybe

strToInt :: String -> Int
strToInt = read

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

splitOn :: Char -> String -> [String]
splitOn delim = wordsWhen ((==) delim)

intLinesFromFile :: String -> IO [Int]
intLinesFromFile file = do
    input <- readFile file
    let numInput = map strToInt (lines input)
    return numInput

intRowFromFile :: String -> IO [Int]
intRowFromFile file = do
    input <- readFile file
    let numInput = map strToInt (splitOn ',' input)
    return numInput

strLinesFromFile :: String -> IO [String]
strLinesFromFile file = do
    input <- readFile file
    let lineInput = (lines input)
    return lineInput

zeroIfNegative :: Int -> Int
zeroIfNegative = max 0

replaceAt :: [a] -> Int -> a -> [a]
replaceAt l pos v = 
    let (x, y) = splitAt pos l
    in x ++ v : (drop 1 y)

replaceIn :: a -> Int -> [a] -> [a]
replaceIn v pos l = replaceAt l pos v 

-- Is inclusive, ie true if test is equal to start or end
isBetween :: Ord a => a -> a -> a -> Bool
isBetween start end test =
    if start <= end then (start <= test) && (test <= end)
                    else (start >= test) && (test >= end)
 

allTwoArgOutputs :: [a] -> [b] -> (a -> b -> c) -> [c]
allTwoArgOutputs as bs f = 
    let fa a = map (f a) bs in
        concat (map fa as)

allTuples :: [a] -> [b] -> [(a, b)]
allTuples as bs = 
    let toTuple a b = (a, b) in
        allTwoArgOutputs as bs toTuple

leftToMaybe :: Either a b -> Maybe a
leftToMaybe e = case e of
    Left a -> Just a
    Right _ -> Nothing

maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft x m = case m of
    Just a -> Left a
    Nothing -> Right x

addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybe a b = if (isNothing a) || (isNothing b) 
    then Nothing
    else Just ((fromJust a) + (fromJust b))