module Old where

import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (Foldable (fold))
import Test.QuickCheck (Property, (==>))

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' x = head x + sum' (tail x)

removeUppercase :: String -> String
removeUppercase str = [c | c <- str, c `elem` ['a' .. 'z']]

reLU :: (Ord p, Num p) => p -> p
reLU x = if x > 0 then x else 0

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

replicate :: (Num a1, Enum a1) => a1 -> a2 -> [a2]
replicate n x = map (const x) [1 .. n]

listWithIndex :: [b] -> [(Int, b)]
listWithIndex = zip [0 ..]

power :: Num a => Int -> a -> a
power y x = product $ replicate' y x

polynomial :: Int -> [Int] -> Int
polynomial x [c] = c * power 0 x
polynomial x coef = power (length coef - 1) x * last coef + polynomial x (init coef)

argMax' :: [Int] -> [Int]
argMax' x = foldr (\x' acc -> if snd x' == argmax then fst x' : acc else acc) [] $ listWithIndex x
  where
    argmax = maximum' x

doubleFactorial :: (Eq p, Num p) => p -> p
doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * doubleFactorial (n - 2)

checkDivisor :: Integral a => a -> a
checkDivisor n
  | fizz && buzz = n
  | fizz = n
  | buzz = n
  | otherwise = 0
  where
    fizz = n `mod` 3 == 0
    buzz = n `mod` 5 == 0

fizzBuzz :: Integral b => b -> [b]
fizzBuzz n = map checkDivisor [1 .. n]

fizzBuzzSum :: Integral a => a -> a
fizzBuzzSum = sum . fizzBuzz

type Rule = (Int, String)

-- generate an infinite cycle from a Rule
-- e.g. (3,"Fizz") -> ["","","Fizz","","","Fizz",...]
cycleFromRule :: Rule -> [String]
cycleFromRule (length, text) =
  let cycle' list = list ++ cycle' list
   in cycle' $ Old.replicate (length -1) "" ++ [text]

-- generate an infinite list of words from a list of rules
-- e.g. [(3,"Fizz"),(5,"Buzz")] -> ["","","Fizz","","Buzz","Fizz",...]
listFromRules :: [Rule] -> [String]
listFromRules rules =
  let zipWithMany f = foldl1 (zipWith f)
      cycles = map cycleFromRule rules
   in zipWithMany (++) cycles
