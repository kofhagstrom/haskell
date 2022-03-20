module MyLib where

import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (Foldable (fold))
import Test.QuickCheck (Property, (==>))

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

reLU :: (Ord p, Num p) => p -> p
reLU x = if x > 0 then x else 0

removeUppercase :: String -> String
removeUppercase str = [c | c <- str, c `elem` ['a' .. 'z']]

removeUppercase' :: [Char] -> [Char]
removeUppercase' = filter (\x -> x `elem` ['a' .. 'z'])

factorial :: (Integral n) => n -> n
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sum' :: (Num a) => [a] -> a
sum' = foldl1 (+)

bmi :: (RealFloat a) => a -> a -> a
bmi height weight = weight / height ^ 2

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell height weight
  | bmi height weight <= 17.5 = "Underweight!"
  | otherwise = "Not underweight!"

fibonacci :: (Integral n) => n -> n
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n -2)

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

prop_reverse' :: Eq a => [a] -> Bool
prop_reverse' x = (reverse'' . reverse'') x == x

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

replicate' :: (Num a1, Enum a1) => a1 -> a2 -> [a2]
replicate' n x = map (const x) [1 .. n]

power :: Num a => Int -> a -> a
power y x = product $ replicate' y x

polynomial :: Int -> [Int] -> Int
polynomial x [c] = c * power 0 x
polynomial x coef = power (length coef - 1) x * last coef + polynomial x (init coef)

square :: Integer -> Integer
square = power 2

tailLong :: Int -> [a] -> [a]
tailLong n = foldr (\x acc -> if length acc == n then acc else x : acc) []

headLong :: Int -> [a] -> [a]
headLong n = reverse'' . tailLong n . reverse''

headLong' :: Int -> [a] -> [a]
headLong' = take

tailLong' :: Int -> [a] -> [a]
tailLong' n = headLong' n . reverse

prop_headLong :: Int -> [a] -> Property
prop_headLong n x = not (null x) ==> length (headLong n x) == n

listWithIndex :: [b] -> [(Int, b)]
listWithIndex x = zip [0 .. length x - 1] x

argMax' :: [Int] -> [Int]
argMax' x = foldr (\x' acc -> if snd x' == argmax then fst x' : acc else acc) [] $ listWithIndex x
  where
    argmax = maximum' x

allOccurences' :: Eq b => b -> [b] -> [Int]
allOccurences' n = map fst . foldr (\x acc -> if snd x == n then x : acc else acc) [] . listWithIndex

product' :: (Num a) => [a] -> a
product' = foldl1 (\acc x' -> acc * x')

factorial' :: (Num a, Enum a) => a -> a
factorial' n = product' [1..n]

doubleFactorial' :: Integral p => p -> p
doubleFactorial' n = if even n then product' . filter even $ [1..n] else product' . filter odd $ [1..n]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []
