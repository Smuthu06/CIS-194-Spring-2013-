module Week4 where

import Data.List

-- Wholemeal Programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- Using fold function
func1 :: [Integer] -> Integer
func1 xs  = foldr (\x y -> if (even x) then (x-2) * y else y ) 1 xs

-- Using function from modules
fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> (x-2)) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even. takeWhile (>1) . iterate collatz
    where
    collatz x = if (even x) then div x 2 else (3 *x+1)

data Tree a = Leaf | Node Int (Tree a) a (Tree a) deriving (Eq, Show)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree a Leaf = Node 0 Leaf a Leaf
insertTree a (Node h lhs root rhs)
    | height lhs <= height rhs = Node (height(insertTree a lhs)+1) (insertTree a lhs) root rhs
    | otherwise                = Node (height(insertTree a rhs)+1) lhs root (insertTree a rhs)
    where
    height Leaf = (-1)
    height (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldl (\x y -> (not x && y) || (x && not y) ) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x b -> f b x ) base (reverse xs)

seiveSundaram :: Integer -> [Integer]
seiveSundaram n = [2*x+1 | x <- genSeive n]
    where
    genCrossed m = [i+j+2*i*j | (i,j) <- filter vaild $ cartProd [1..m] [1..m]]
    vaild (i,j)  = i <= j && ((i +j +2*i*j) < n)
    genSeive  n  =  [1..n]  \\ genCrossed n

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
