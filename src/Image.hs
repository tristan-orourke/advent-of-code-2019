module Image (
        pixelsToLayers,
        layerWithFewestX
    ) where

import Util
import Data.List (minimumBy)

pixelsToLayers :: Int -> Int -> [Int] -> [[Int]]
pixelsToLayers w h xs = splitEveryN (w*h) xs

layerWithFewestX :: Eq a => a -> [[a]] -> [a]
layerWithFewestX x xs = minimumBy (compareByCountOf x) xs

compareByCountOf :: Eq a => a -> [a] -> [a] -> Ordering
compareByCountOf a x y = 
    let cx = countIn a x
        cy = countIn a y in
            compare cx cy