module Orbits (
    orbitOfStr,
    totalDirectIndirectOrbits,
    countOrbitJumps
) where

import Tree
import Util
import Data.Maybe

orbitOfStr :: String -> Tree String
orbitOfStr s = let [a, b] = take 2 (splitOn ')' s) in
    Node a [Node b []]

orbitTreeOfStrs :: [String] -> Tree String
orbitTreeOfStrs = head . mergeForest . (map orbitOfStr)

totalDirectIndirectOrbits :: [String] -> Int
totalDirectIndirectOrbits = countEdges . orbitTreeOfStrs

countOrbitJumps :: String -> String -> [String] -> Int
countOrbitJumps a b = ((+) (-2)) . fromJust . (minDistanceT a b) . orbitTreeOfStrs

-- >>> x = ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"] 
-- >>> countEdges $ orbitTreeOfStrs x
-- 42
--
