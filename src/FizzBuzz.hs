type Rule = (Int, String)

makeCycle :: Rule -> [String]
makeCycle (length, text) = cycle $ replicate (length -1) "" ++ [text]

generalZipWith :: Foldable t => (b -> b -> b) -> t [b] -> [b]
generalZipWith f = foldl1 (zipWith f)

words' :: [Rule] -> [String]
words' rules = generalZipWith (++) $ map makeCycle rules

generalFizzBuzz :: [Rule] -> [String]
generalFizzBuzz rules = zipWith max nums $ words' rules
    where nums = map show [1 ..]

fizzBuzz :: [String]
fizzBuzz = generalFizzBuzz [(3, "Fizz"), (5,"Buzz")]
