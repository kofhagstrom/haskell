module Words where

reverse' :: [a] -> [a]
reverse' [] = error "reverse' of empty list"
reverse' [x] = [x]
reverse' (x : xs) = reverse' xs ++ [x]

words' :: [Char] -> [Char]
words' [] = error "words' of empty string"
words' [x] = [x]
words' (x:xs) = if x==' ' then xs else words' (x:xs)

revWords :: String -> String
revWords = unwords . reverse' . words