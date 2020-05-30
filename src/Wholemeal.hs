module Wholemeal where

import Data.List

fun1 :: [Integer] -> Integer
fun1 = product . map ((-) 2). filter even

fun2 :: Integer -> Integer
fun2 = 
    let fn n = if even n then n `div` 2 else 3 * n + 1
    in sum . filter even . takeWhile (>1) . iterate fn


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree xs = foldr insert Leaf xs where 
    insert a Leaf = Node 1 Leaf a Leaf
    insert a n@(Node h Leaf na Leaf) = Node (h + 1) n a Leaf
    insert a n@(Node h l na r) 
        | hl > hr = Node h l a (insert a r)
        | hr > hl = Node h (insert a l) a r
        | otherwise = Node (nh + 1) newTree a r
        where
            hl = treeHeight l
            hr = treeHeight r
            newTree = insert a l    
            nh = treeHeight newTree

xor :: [Bool] -> Bool
xor = foldr (/=) False 

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x:acc) [] xs