module Util
    ( 
        strToInt,
        intLinesFromFile,
        intRowFromFile,
        zeroIfNegative,
        replaceAt,
        replaceIn
    ) where

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


zeroIfNegative :: Int -> Int
zeroIfNegative = max 0

replaceAt :: [a] -> Int -> a -> [a]
replaceAt l pos v = 
    let (x, y) = splitAt pos l
    in x ++ v : (drop 1 y)

replaceIn :: a -> Int -> [a] -> [a]
replaceIn v pos l = replaceAt l pos v 

