module Old where

import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (Foldable (fold))
import Test.QuickCheck (Property, (==>))

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' x = head x + sum' (tail x)

firstOccurence :: (Eq t, Num p) => t -> [t] -> p
firstOccurence n [] = error "firstOccurence of empty list"
firstOccurence n [x] = 0
firstOccurence n all@(x : xs)
  | x == n = 0
  | otherwise = 1 + firstOccurence n xs

prop_firstOccurence :: Eq a => a -> [a] -> Property
prop_firstOccurence n x = not (null x) ==> x !! firstOccurence n x == n

splitAtFirstOccurence :: Eq a => a -> [a] -> ([a], [a])
splitAtFirstOccurence n x = second tail split
  where
    split = splitAt (firstOccurence n x) x

allOccurences :: (Eq a, Num a) => a -> [a] -> [a]
allOccurences n [x]
  | n == x = [0]
  | otherwise = []
allOccurences n [] = []
allOccurences n x = first : map (1 + first +) (allOccurences n ((snd . splitAtFirstOccurence n) x))
  where
    first = firstOccurence n x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list."
maximum' [x] = x
maximum' (x : x' : xs)
  | x > x' = maximum' (x : xs)
  | otherwise = maximum' (x' : xs)

argMax :: (Num p, Ord a) => [a] -> p
argMax [] = error "argMax of empty list."
argMax [x] = 0
argMax (x : xs)
  | x > maxTail = 0
  | otherwise = 1 + argMax xs
  where
    maxTail = maximum' xs

tailLong :: Int -> [a] -> [a]
tailLong n [] = error "tailLong of empty list."
tailLong 1 x = [last x]
tailLong n all@(x : xs)
  | length all == n = all
  | otherwise = tailLong n xs

replicate' :: Int -> a -> [a]
replicate' 1 x = [x]
replicate' n x = x : replicate' (n -1) x

headLong' :: Int -> [a] -> [a]
headLong' n [] = error "headLong' of empty list."
headLong' 1 x = [head x]
headLong' n all@(x : xs)
  | length all == n = all
  | otherwise = x : headLong' n xs
