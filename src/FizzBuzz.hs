checkDivisor :: Integral a => a -> a
checkDivisor n
    | fizz && buzz = n
    | fizz = n
    | buzz = n
    | otherwise = 0
    where fizz = n `mod` 3 == 0
          buzz = n `mod` 5 == 0

fizzBuzz :: Integral b => b -> [b]
fizzBuzz n = map checkDivisor [1..n]

fizzBuzzSum :: Integral a => a -> a
fizzBuzzSum = sum . fizzBuzz

