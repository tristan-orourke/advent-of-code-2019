module Wires
    (
        Wire (..),
        Point (..),
        lengthToPoint,
        newWire,
        toCoords,
        extendPathSeg,
        fromPath,
        fromPathStr,
        manhattanDist,
        tipDistance,
        smallestIntersectDistOfPaths,
        lowestIntersectionLengthOfPaths,
    ) where

import Util
import Data.List
import Data.Maybe
import Control.Monad

class Vector a where
    add, sub :: a -> a -> a
    mult :: a -> Int -> a

data Point = Pt Int Int
instance Vector Point where
    add (Pt a b) (Pt x y) = Pt (a+x) (b+y) 
    sub (Pt a b) (Pt x y) = Pt (a-x) (b-y)
    mult (Pt a b) l = Pt (a*l) (b*l)
instance Eq Point where
    (Pt a b) == (Pt x y) = (a == x) && (b == y)
instance Show Point where
    show (Pt a b) = show (a, b)

class Path a where
    tip, origin :: a -> Point
    endpoints :: a -> (Point, Point)
    intersections :: a -> a -> [Point]
    len :: a -> Int
    pointIntersects :: a -> Point -> Bool
    lengthToPoint :: a -> Point -> Maybe Int

data Direction = Up | Down | L | R
    deriving (Eq, Show)
data Axis = Horizontal | Vertical
    deriving (Eq, Show)
data WireSegment = WS Point Direction Int
    deriving (Eq, Show)
    
dirAxis :: Direction -> Axis
dirAxis Up = Vertical
dirAxis Down = Vertical
dirAxis L = Horizontal
dirAxis R = Horizontal

segmentAxis :: WireSegment -> Axis
segmentAxis (WS _ d _) =  dirAxis d

parallelDirs :: Direction -> Direction -> Bool
parallelDirs a b = (dirAxis a) == (dirAxis b)

parallelSegments :: WireSegment -> WireSegment -> Bool
parallelSegments a b =
    let getDir (WS _ d _) = d in
        parallelDirs (getDir a) (getDir b)

segmentEndpoints :: WireSegment -> (Point, Point)
segmentEndpoints (WS o d l) = (o, o `add` ((unitVec d) `mult` l))

returnHorizontalFirst :: WireSegment -> WireSegment -> (WireSegment, WireSegment)
returnHorizontalFirst a b =
    if (segmentAxis a) == Horizontal then (a, b) else (b, a)

-- Assumptions: wire segments will never fully overlap (intersect in parallel)
segmentIntersections :: WireSegment -> WireSegment -> [Point]
segmentIntersections a b =
    if parallelSegments a b then []
    else let (wx, wy) = returnHorizontalFirst a b in
        let ((Pt x1 y'), (Pt x2 _)) = segmentEndpoints wx
            ((Pt x' y1), (Pt _ y2)) = segmentEndpoints wy in
                if (isBetween x1 x2 x') && (isBetween y1 y2 y')
                    then [Pt x' y']
                    else []

instance Path WireSegment where
    tip (WS o d l) = o `add` ((unitVec d) `mult` l)
    origin (WS o _ _) = o
    endpoints w = (origin w, tip w)
    intersections = segmentIntersections
    len (WS _ _ l) = l
    pointIntersects w (Pt x' y') = 
        let ((Pt x1 y1), (Pt x2 y2)) = endpoints w in
            if segmentAxis w == Horizontal
                then y' == y1 && (isBetween x1 x2 x')
                else x' == x1 && (isBetween y1 y2 y')
    lengthToPoint w p = if pointIntersects w p then Just (manhattanDist (origin w) p) else Nothing

data Wire = Wire [WireSegment]
    deriving Eq
instance Show Wire where
    show (Wire ws) = show (map tip ws)    
instance Path Wire where
    origin w = originP
    tip w = 
        let (Wire ws) = w in
            if ws == [] then origin w
            else tip (head ws)
    endpoints w = (origin w, tip w)
    intersections (Wire as) (Wire bs) = delete originP (concat (allTwoArgOutputs as bs intersections))
    len (Wire ws) = sum (map len ws)
    pointIntersects (Wire ws) p = any ((flip pointIntersects) p) ws
    lengthToPoint (Wire ws) p = leftToMaybe $ foldM (lengthToPoint') 0 (reverse ws)
        where 
            lengthToPoint' :: Int -> WireSegment -> Either Int Int
            lengthToPoint' d w = case lengthToPoint w p of
                Just n -> Left (d + n)
                Nothing -> Right (d + len w)            

originP :: Point
originP = Pt 0 0

newWire :: Wire
newWire = Wire []

pointFromTuple :: (Int, Int) -> Point
pointFromTuple (x, y) = Pt x y

pointToTuple :: Point -> (Int, Int)
pointToTuple (Pt x y) = (x, y)

toPoints :: Wire -> [Point]
toPoints (Wire ws) = map tip ws

toCoords :: Wire -> [(Int, Int)]
toCoords (Wire ws) = map pointToTuple (map tip ws)

addFromTip :: Wire -> Point -> Point
addFromTip w = add (tip w)

addSegment :: WireSegment -> Wire -> Wire
addSegment w (Wire ws) = Wire (w:ws)

unitVec :: Direction -> Point
unitVec Up = Pt 0 1
unitVec Down = Pt 0 (-1)
unitVec L = Pt (-1) 0
unitVec R = Pt 1 0

extendLength :: Direction -> Int -> Wire -> Wire
extendLength d 0 w = w
extendLength d l w = 
    let o = tip w in
        addSegment (WS o d l) w

charToDir :: Char -> Direction
charToDir 'U' = Up
charToDir 'D' = Down
charToDir 'L' = L
charToDir 'R' = R
charToDir _ = error "Invalid Direction Character"

extendPathSeg :: String -> Wire -> Wire
extendPathSeg s =
    let d:l = s in
        extendLength (charToDir d) (strToInt l)

extendPath :: Wire -> [String] -> Wire
extendPath w [] = w
extendPath w (x:xs) = extendPath (extendPathSeg x w) xs

fromPath :: [String] -> Wire
fromPath = extendPath newWire

fromPathStr :: String -> Wire
fromPathStr = fromPath . (splitOn ',')

manhattanDist :: Point -> Point -> Int
manhattanDist (Pt x1 y1) (Pt x2 y2) = (abs (x1 - x2)) + (abs (y1 - y2))

tipDistance :: Path a => a -> Int
tipDistance = (manhattanDist originP) . tip

closestIntersectionDist :: Path a => a -> a -> Int
closestIntersectionDist a b = foldr1 min $ map (manhattanDist originP) (intersections a b)

lowestInstersectionLength :: Path a => a -> a -> Int
lowestInstersectionLength a b = 
    let ps = intersections a b
        totalLength p = (lengthToPoint a p) `addMaybe` (lengthToPoint b p) in
            foldr1 min $ catMaybes $ map totalLength ps

wireFuncOfPaths :: (Wire -> Wire -> a) -> String -> String -> a
wireFuncOfPaths f a b = 
    let x = fromPathStr a
        y = fromPathStr b in
        f x y

smallestIntersectDistOfPaths :: String -> String -> Int
smallestIntersectDistOfPaths = wireFuncOfPaths closestIntersectionDist

lowestIntersectionLengthOfPaths :: String -> String -> Int
lowestIntersectionLengthOfPaths = wireFuncOfPaths lowestInstersectionLength

-- >>> len $ fromPathStr "U100,L10,D40,R20"
-- >>> x = fromPathStr "U100,L10,D40,R30"
-- >>> lengthToPoint x $ tip x
-- 170
-- Just 30
--
