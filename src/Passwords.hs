module Passwords
    (
        isValid,
        countValidInRange,
        isValidStrict,
        countValidStrictInRange
    ) where

import Util
import Data.Either (either)
import Control.Monad (foldM)

isSixDigits :: Integral a => a -> Bool
isSixDigits = isBetween 100000 999999

adjacentDups :: Eq a => [a] -> [Bool]
adjacentDups xs = zipWith (==) xs (tail xs)

hasAdjacentDupsL :: Eq a => [a] -> Bool
hasAdjacentDupsL = or . adjacentDups

hasAdjacentDups :: Int -> Bool
hasAdjacentDups = hasAdjacentDupsL . digits

hasDupTripleL :: Eq a => [a] -> Bool
hasDupTripleL xs = or $ zipWith3 eq3 xs (tail xs) (tail $ tail xs)
    where eq3 a b c = a == b && b == c

data SeqBool = F | FirstT | RepeatT | LoneT deriving Eq

hasLoneTrue :: [Bool] -> Bool
hasLoneTrue bs = 
    let 
        f :: SeqBool -> Bool -> SeqBool
        f LoneT _ = LoneT
        f FirstT False = LoneT
        f FirstT True = RepeatT
        f RepeatT True = RepeatT
        f RepeatT False = F
        f F True = FirstT
        f F False = F
        goodSeq b = b == FirstT || b == LoneT
    in goodSeq $ foldl f F bs

-- >>> x = adjacentDups $ digits 123444
-- >>> x
-- >>> hasLoneTrue x
-- [False,False,False,True,True]
-- False
--

-- Unlike hasAdjacentDups, this will only succeed if there is an adjacent set of exact length 2
hasDupExactPairL :: Eq a => [a] -> Bool
-- hasDupExactPairL xs = hasAdjacentDupsL xs && not (hasDupTripleL xs)
hasDupExactPairL = hasLoneTrue . adjacentDups
    


hasDupExactPair :: Int -> Bool
hasDupExactPair = hasDupExactPairL . digits

isAscending :: Ord a => [a] -> Bool
isAscending xs = and $ zipWith (<=) xs (tail xs)

digitsNeverDecrease :: Integral a => a -> Bool
digitsNeverDecrease = isAscending . digits

isValid :: Int -> Bool
isValid = and . sequenceA [isSixDigits, hasAdjacentDups, digitsNeverDecrease]

isValidStrict :: Int -> Bool
isValidStrict = and . sequenceA [isSixDigits, hasDupExactPair, digitsNeverDecrease]

countValidInRange' :: (Int -> Bool) -> Int -> Int -> Int
countValidInRange' f min max = length $ filter (==True) $ map f [min..max]

countValidInRange :: Int -> Int -> Int
countValidInRange = countValidInRange' isValid

countValidStrictInRange :: Int -> Int -> Int
countValidStrictInRange = countValidInRange' isValidStrict

