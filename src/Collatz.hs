{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}
module Collatz where

collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n = if even n then n : collatzChain (n `div` 2) else n : collatzChain (3 * n + 1)

lengthOfChains :: Integral a => a -> [Int]
lengthOfChains 1 = [1]
lengthOfChains n = lengthOfChains (n - 1) ++ [length (collatzChain n)]

maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum' of empty list."
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

maximum'' :: Ord a => [a] -> a
maximum'' = foldl1 (\acc x -> if x > acc then x else acc)

longestChain :: Integer -> Int
longestChain = maximum' . lengthOfChains

map' :: (t -> a) -> [t] -> [a]
map' f [] = []
map' f (x : xs) = f x : map' f xs

map'' :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map'' f = foldr (\x acc -> f x : acc) []
