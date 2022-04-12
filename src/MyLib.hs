module MyLib where

import Test.QuickCheck (Property, (==>))

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

reLU :: Integer -> Integer
reLU = max 0

removeUppercase :: [Char] -> [Char]
removeUppercase = filter (\x -> x `elem` ['a' .. 'z'])

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
maximum' = foldl1 (\acc x -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

prop_reverse' :: Eq a => [a] -> Bool
prop_reverse' x = (reverse' . reverse') x == x

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

doubleFactorial :: Integral p => p -> p
doubleFactorial n
    | even n = product' $ filter even [1..n]
    | otherwise = product' $ filter odd [1..n]

tailLong :: Int -> [a] -> [a]
tailLong n = foldr (\x acc -> if length acc == n then acc else x : acc) []

headLong :: Int -> [a] -> [a]
headLong n = reverse' . tailLong n . reverse'

headLong' :: Int -> [a] -> [a]
headLong' = take

tailLong' :: Int -> [a] -> [a]
tailLong' n = headLong' n . reverse

prop_headLong :: Int -> [a] -> Property
prop_headLong n x = not (null x) ==> length (headLong n x) == n

listWithIndex :: [b] -> [(Int, b)]
listWithIndex = zip [0..]

allOccurences :: Eq b => b -> [b] -> [Int]
allOccurences n = map fst . filter (\x -> snd x == n) . listWithIndex

argMax :: Ord b => [b] -> [Int]
argMax x = allOccurences (maximum' x) x

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

factorial' :: (Num a, Enum a) => a -> a
factorial' n = product' [1..n]
