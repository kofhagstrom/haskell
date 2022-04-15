module MinMaxSum where

import Data.List (sort)

calcMax :: (Num b, Ord b) => [b] -> [b]
calcMax list = map (sum . tail) [reverse sorted, sorted]
  where
    sorted = sort list
    
main :: IO ()
main = do
  s <- getLine
  putStrLn $ unwords . map show $ calcMax $ map read (words s)
