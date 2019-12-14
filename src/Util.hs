module Util where

import Data.Maybe
import Control.Applicative

strToInt :: String -> Int
strToInt = read

charToInt :: Char -> Int
charToInt c = read [c]

intToStr :: Int -> String
intToStr = show

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

splitOn :: Char -> String -> [String]
splitOn delim = wordsWhen ((==) delim)

splitEveryN :: Int -> [a] -> [[a]]
splitEveryN _ [] = []
splitEveryN n x = (take n x) : (splitEveryN n (drop n x))


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

digitsFromFile :: String -> IO [Int]
digitsFromFile file = do
    input <- readFile file
    return (map charToInt input)

-- >>> map charToInt "1234"
-- [1,2,3,4]
--

zeroIfNegative :: Int -> Int
zeroIfNegative = max 0

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs pos v = 
    let (x, y) = splitAt pos xs
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

-- Nothing becomes the default Left a
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = maybeToRight

-- Left _ becomes Nothing
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = rightToMaybe

leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft = flip maybe Left . Right

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight = flip maybe Right . Left

addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybe = liftA2 (+)

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10] 

intOfDigits :: [Int] -> Int
intOfDigits xs = 
    let 
        intOfDigits' :: Int -> [Int] -> Int
        intOfDigits' t xs = case xs of
            [] -> t
            (d:ds) -> intOfDigits' (t+d) (map (*10) ds)
        in intOfDigits' 0 (reverse xs)

safeL :: ([a] -> [a]) -> [a] -> [a]
safeL _ [] = []
safeL f xs = f xs

safeTail :: [a] -> [a]
safeTail = safeL tail

safeInit :: [a] -> [a]
safeInit = safeL init

-- | Pads xs with p (on the left) if n > length xs.
-- | Returns xs unchanged if the length is higher than n.
lpad :: a -> Int -> [a] -> [a]
lpad p n xs = replicate (n - length ys) p ++ xs
    where ys = take n xs

-- | Pads xs with p (on the right) if n > length xs.
-- | Returns xs unchanged if the length is higher than n.
-- | This will fail on infinite lists.
rpad :: a -> Int -> [a] -> [a]
rpad p n xs = xs ++ replicate (n - length xs) p

minMaybe :: Ord a => [Maybe a] -> Maybe a
minMaybe ms = case catMaybes ms of
    [] -> Nothing
    xs -> Just (minimum xs)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)

tuplify4 :: [a] -> (a,a,a,a)
tuplify4 [w,x,y,z] = (w,x,y,z)

uncurry3 :: (a -> a -> a -> b) -> (a,a,a) -> b
uncurry3 f (x,y,z) = f x y z

uncurry4 :: (a -> a -> a -> a -> b) -> (a,a,a,a) -> b
uncurry4 f (w,x,y,z) = f w x y z

countIn :: Eq a => a -> [a] -> Int
countIn x = length . filter (==x)
