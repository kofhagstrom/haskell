{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module FizzBuzz (generalFizzBuzz, fizzBuzz, fizzBuzzSazz) where

type Rule = (Int, String)

type Cycle = [String]

cycleFromRule :: Rule -> Cycle
cycleFromRule (length, text) = cycle $ replicate (length -1) "" ++ [text]

-- generate an infinite list consisting of generalized fizzbuzz terms from list of Rules
-- e.g. [(3,"Fizz"),(5,"Buzz")] -> ["1","2","Fizz","4","Buzz","Fizz",...]
generalFizzBuzz :: [Rule] -> [String]
generalFizzBuzz rules =
  let nums = map show [1 ..]
      words = listFromRules rules
        where
          listFromRules rules =
            let cycles = map cycleFromRule rules
             in foldl1 (zipWith (++)) cycles
   in zipWith max nums words

-- special case
-- returns ["1","2","Fizz","4","Buzz","Fizz",...]
fizzBuzz :: [String]
fizzBuzz = generalFizzBuzz [(3, "Fizz"), (5, "Buzz")]

-- special case
-- returns ["1","2","Fizz","4","Buzz","Fizz","Sazz","8",...]
fizzBuzzSazz :: [String]
fizzBuzzSazz = generalFizzBuzz [(3, "Fizz"), (5, "Buzz"), (7, "Sazz")]

length' :: Num p => [a] -> p
length' = foldr (\a -> (+) 1) 0

plusMinus :: (Fractional a1, Ord a2, Num a2) => [a2] -> [a1]
plusMinus arr =
  let pos = length' $ filter (> 0) arr
      zero = length' $ filter (== 0) arr
      neg = length' $ filter (< 0) arr
      len = length' arr
   in [pos / len, neg / len, zero / len]