type Rule = (Int, String)

-- generate an infinite cycle of list
cycle' :: [a] -> [a]
cycle' list = list ++ cycle' list

-- generate an infinite cycle from a Rule
-- e.g. (3,"Fizz") -> ["","","Fizz","","","Fizz",...]
ruleToCycle :: Rule -> [String]
ruleToCycle (length, text) = cycle' $ replicate (length -1) "" ++ [text]

-- generate an infinite list of words from a list of rules
-- e.g. [(3,"Fizz"),(5,"Buzz")] -> ["","","Fizz","","Buzz","Fizz",...]
rulesToWords :: [Rule] -> [String]
rulesToWords rules = generalZipWith (++) $ map ruleToCycle rules
  where
    generalZipWith f = foldl1 (zipWith f)

-- generate an infinite list consisting of generalized fizzbuzz terms from list of Rules
-- e.g. [(3,"Fizz"),(5,"Buzz")] -> ["1","2","Fizz","4","Buzz","Fizz",...]
generalFizzBuzz :: [Rule] -> [String]
generalFizzBuzz rules = zipWith max nums $ rulesToWords rules
  where
    nums = map show [1 ..]

-- special case
-- returns ["1","2","Fizz","4","Buzz","Fizz",...]
fizzBuzz :: [String]
fizzBuzz = generalFizzBuzz [(3, "Fizz"), (5, "Buzz")]

-- special case
-- returns ["1","2","Fizz","4","Buzz","Fizz","Sazz",8,...]
fizzBuzzSazz :: [String]
fizzBuzzSazz = generalFizzBuzz [(3, "Fizz"), (5, "Buzz"), (7, "Sazz")]

