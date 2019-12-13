module Image (
        digitsToLayers,
        layerWithFewestX,
        renderLayers,
        drawPixels,
    ) where

import Util
import Data.List (minimumBy)

digitsToLayers :: Int -> Int -> [Int] -> [[Int]]
digitsToLayers w h xs = splitEveryN (w*h) xs

layerWithFewestX :: Eq a => a -> [[a]] -> [a]
layerWithFewestX x xs = minimumBy (compareByCountOf x) xs

compareByCountOf :: Eq a => a -> [a] -> [a] -> Ordering
compareByCountOf a x y = 
    let cx = countIn a x
        cy = countIn a y in
            compare cx cy

data Pixel = Black | White | Transparent deriving (Show, Eq)

pixelOfInt :: Int -> Pixel
pixelOfInt 0 = Black
pixelOfInt 1 = White
pixelOfInt _ = Transparent

charOfPixel :: Pixel -> Char
charOfPixel Black = ' '
charOfPixel White = '+'
charOfPixel Transparent = ' '

-- | a and b represent pixels in two layers, with a rendered in front of b.
renderPixel :: Pixel -> Pixel -> Pixel
renderPixel Transparent b = b
renderPixel a _ = a

renderLayers :: [[Int]] -> [Pixel]
renderLayers xs = foldr1 (zipWith renderPixel) (map (map pixelOfInt) xs) 

drawPixels :: Int -> [Pixel] -> String
drawPixels w ps = 
    let rs = splitEveryN w ps 
        pixelsToStr = map charOfPixel in
            unlines $ map pixelsToStr rs


-- >>> x =renderLayers [[0,1,1,1,0,0],[0,0,1,0,0,0]]
-- >>> drawPixels 2 x
-- " +\n++\n  \n"
--
