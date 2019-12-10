module Tree (
    module Tree,
    module Data.Tree
) where

import Data.Tree

treeHas :: Eq a => a -> Tree a -> Bool
treeHas x (Node a ts) = (x == a) || any (treeHas x) ts

treesOverlap :: Eq a => Tree a -> Tree a -> Bool
treesOverlap a b = treeHas (rootLabel b) a || treeHas (rootLabel a) b

-- | inserts x into `into` if they overlap. Simply returns `into` if it does not contain the rootLabel of x.
insertT :: Eq a => Tree a -> Tree a -> Tree a
insertT x into = if rootLabel x == rootLabel into
    then Node (rootLabel into) (subForest x ++ subForest into)
    else Node (rootLabel into) (map (insertT x) (subForest into))

insertTM :: Eq a => Tree a -> Tree a -> Maybe (Tree a)
insertTM x into = if treeHas (rootLabel x) into 
    then Just (insertT x into)
    else Nothing

-- TODO: rewrite
joinTreesM :: Eq a => Tree a -> Tree a -> Maybe (Tree a)
joinTreesM a b = case insertTM a b of
    Just t -> Just t
    Nothing -> case insertTM b a of
        Just y -> Just y
        Nothing -> Nothing

joinWithForest :: Eq a => Forest a -> Tree a -> Forest a
joinWithForest [] x = [x]
joinWithForest (t:ts) x = case joinTreesM x t of
    Just y -> y:ts
    Nothing -> t:joinWithForest ts x

mergeForest :: Eq a => Forest a -> Forest a
mergeForest ts = 
    let ts' = foldl joinWithForest [] ts in
        if length ts == length ts' 
            then ts'
            else mergeForest ts'

countEdges :: Tree a -> Int
countEdges t =
    let countEdges' d (Node _ ts) = d + sum (map (countEdges' (d+1)) ts) 
        in countEdges' 0 t

-- >>> t = Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]
-- >>> t
-- >>> treeHas 3 t
-- >>> treeHas 7 t
-- >>> x = Node 7 [Node 8 []]
-- >>> y = Node 3 [Node 10 [], Node 6 [Node 7 []]]
-- >>> z = Node 100 []
-- >>> zz = Node 200 []
-- >>> treesOverlap t x
-- >>> treesOverlap t y
-- >>> insertTM x t
-- >>> insertTM y t
-- >>> printForest t = putStr $ drawForest $ map (fmap show) t
-- >>> printTree t = putStr $ drawTree $ fmap show t
-- >>> printTree t
-- Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest = []},Node {rootLabel = 5, subForest = []}]}]}
-- True
-- False
-- False
-- True
-- Nothing
-- Just (Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = [Node {rootLabel = 10, subForest = []},Node {rootLabel = 6, subForest = [Node {rootLabel = 7, subForest = []}]},Node {rootLabel = 4, subForest = []},Node {rootLabel = 5, subForest = []}]}]})
-- 1
-- |
-- +- 2
-- |
-- `- 3
--    |
--    +- 4
--    |
--    `- 5
--