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

longestChain :: Integer -> Int
longestChain = maximum' . lengthOfChains

map' :: (t -> a) -> [t] -> [a]
map' f [] = []
map' f (x : xs) = f x : map' f xs

reverse' :: [a] -> [a]
reverse' [] = error "reverse' of empty list"
reverse' [x] = [x]
reverse' (x : xs) = reverse' xs ++ [x]

words' :: [Char] -> [Char]
words' [] = error "words' of empty string"
words' [x] = [x]
words' (x:xs) = if x==' ' then xs else words' (x:xs)

revWords :: String -> String
revWords = unwords . reverse' . words
