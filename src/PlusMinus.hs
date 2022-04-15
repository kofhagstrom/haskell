module PlusMinus where

getLengthRatio :: [Int] -> [Int] -> Double
getLengthRatio b a = fromIntegral (length a) / fromIntegral (length b)

plusMinus :: [Int] -> String
plusMinus arr = unlines $ map (show . getLengthRatio arr) [pos, neg, zero]
  where
    pos = filter (> 0) arr
    zero = filter (== 0) arr
    neg = filter (< 0) arr

main :: IO ()
main = interact $ plusMinus . map read . words . last . lines

