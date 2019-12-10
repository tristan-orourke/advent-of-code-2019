module Orbits (
    orbitOfStr,
    totalDirectIndirectOrbits
) where

import Tree
import Util

orbitOfStr :: String -> Tree String
orbitOfStr s = let [a, b] = take 2 (splitOn ')' s) in
    Node a [Node b []]

orbitTreeOfStrs :: [String] -> Tree String
orbitTreeOfStrs = head . mergeForest . (map orbitOfStr)

totalDirectIndirectOrbits :: [String] -> Int
totalDirectIndirectOrbits = countEdges . orbitTreeOfStrs

-- >>> x = ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"] 
-- >>> countEdges $ orbitTreeOfStrs x
-- 42
--
