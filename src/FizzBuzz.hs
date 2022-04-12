module FizzBuzz (generalFizzBuzz, fizzBuzz, fizzBuzzSazz) where

type Rule = (Int, String)

-- generate an infinite cycle from a Rule
-- e.g. (3,"Fizz") -> ["","","Fizz","","","Fizz",...]
cycleFromRule :: Rule -> [String]
cycleFromRule (length, text) =
  let cycle' list = list ++ cycle' list
   in cycle' $ replicate (length -1) "" ++ [text]

-- generate an infinite list of words from a list of rules
-- e.g. [(3,"Fizz"),(5,"Buzz")] -> ["","","Fizz","","Buzz","Fizz",...]
listFromRules :: [Rule] -> [String]
listFromRules rules =
  let zipWithMany f = foldl1 (zipWith f)
      cycles = map cycleFromRule rules
   in zipWithMany (++) cycles

-- generate an infinite list consisting of generalized fizzbuzz terms from list of Rules
-- e.g. [(3,"Fizz"),(5,"Buzz")] -> ["1","2","Fizz","4","Buzz","Fizz",...]
generalFizzBuzz :: [Rule] -> [String]
generalFizzBuzz rules =
  let words = listFromRules rules
      nums = map show [1 ..]
   in zipWith max nums words

-- special case
-- returns ["1","2","Fizz","4","Buzz","Fizz",...]
fizzBuzz :: [String]
fizzBuzz = generalFizzBuzz [(3, "Fizz"), (5, "Buzz")]

-- special case
-- returns ["1","2","Fizz","4","Buzz","Fizz","Sazz","8",...]
fizzBuzzSazz :: [String]
fizzBuzzSazz = generalFizzBuzz [(3, "Fizz"), (5, "Buzz"), (7, "Sazz")]
