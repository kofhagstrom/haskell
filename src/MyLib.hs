module MyLib where

import Data.Bifunctor (Bifunctor (second))
import Test.QuickCheck (Property, (==>))

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

reLU :: (Ord p, Num p) => p -> p
reLU x = if x > 0 then x else 0

removeUppercase :: String -> String
removeUppercase str = [c | c <- str, c `elem` ['a' .. 'z']]

factorial :: (Integral n) => n -> n
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' x = head x + sum' (tail x)

bmi :: (RealFloat a) => a -> a -> a
bmi height weight = weight / height ^ 2

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell height weight
  | bmi height weight <= 17.5 = "Underweight!"
  | otherwise = "Not underweight!"

fibonacci :: (Integral n) => n -> n
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n -1) + fibonacci (n -2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list."
maximum' [x] = x
maximum' (x : x' : xs)
  | x > x' = maximum' (x : xs)
  | otherwise = maximum' (x' : xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: t -> [t]
repeat' x = x : repeat' x

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

g' :: Integer -> Integer
g' x = x + 1

g'' :: Int -> Int
g'' x = x + 1

f :: Int -> Int
f x = x ^ 2

g :: Int -> Int
g x = x + 1

h :: Int -> Int
h = f . g

i :: Int -> Int
i = g . f

j :: Int -> Int
j = f . g . f

volumeCylinder :: Floating a => a -> a -> a
volumeCylinder h r = h * areaCircle r
  where
    areaCircle r = pi * r ^ 2

volumeRect :: (Num a) => a -> a -> a -> a
volumeRect l h w = l * h * w

volumeSquare :: Num a => a -> a
volumeSquare s = volumeRect s s s

xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

doubleFactorial :: (Eq p, Num p) => p -> p
doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * doubleFactorial (n - 2)

power :: (Eq p, Num p) => p -> p -> p
power 0 x = 1
power y x = x * power (y - 1) x

replicate' :: Int -> a -> [a]
replicate' 1 x = [x]
replicate' n x = x : replicate' (n -1) x

polynomial :: Int -> [Int] -> Int
polynomial x [c] = c * power 0 x
polynomial x coef = power (length coef - 1) x * last coef + polynomial x ((tail . reverse) coef)

square :: Integer -> Integer
square = power 2

tailLong :: Int -> [a] -> [a]
tailLong n [] = error "tailLong of empty list."
tailLong 1 x = [(head . reverse') x]
tailLong n all@(x : xs)
  | length all == n = all
  | otherwise = tailLong n xs

headLong :: Int -> [a] -> [a]
headLong n [] = error "headLong of empty list."
headLong n x = reverse' $ tailLong n $ reverse' x

headLong' :: Int -> [a] -> [a]
headLong' n [] = error "headLong' of empty list."
headLong' 1 x = [head x]
headLong' n all@(x : xs)
  | length all == n = all
  | otherwise = x : headLong' n xs

prop_headLong :: Int -> [a] -> Property
prop_headLong n x = not (null x) ==> length (headLong n x) == n

prop_reverse' :: Eq a => [a] -> Bool
prop_reverse' x = (reverse' . reverse') x == x

argMax :: (Num p, Ord a) => [a] -> p
argMax [] = error "argMax of empty list."
argMax [x] = 0
argMax (x : xs)
  | x > maxTail = 0
  | otherwise = 1 + argMax xs
  where
    maxTail = maximum' xs

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