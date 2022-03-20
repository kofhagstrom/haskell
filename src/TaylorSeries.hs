module TaylorSeries where

factorial' :: (Num a, Enum a) => a -> a
factorial' n = product [1..n]

repeat' :: t -> [t]
repeat' x = x : repeat' x

replicate' :: Int -> a -> [a]
replicate' n = take n . repeat'

power :: Num a => Int -> a -> a
power x y = product $ replicate' x y

polynomial :: (Num c, Enum c) => [c] -> Int -> [c]
polynomial coef x = zipWith (curry (\c -> fst c * power x (snd c))) coef [0..]

taylorSeries :: (Num c, Num a, Enum c, Enum a) => (a -> c) -> Int -> [c]
taylorSeries coefGenerator = polynomial $ map coefGenerator [1..]

expFunction :: Int -> [Double]
expFunction = taylorSeries (\x -> x / factorial' x)

eulersNumber :: Int -> Double
eulersNumber nTerms = sum $ take nTerms (expFunction 1)