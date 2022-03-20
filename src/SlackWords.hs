module SlackWords where

formatChar :: Char -> String
formatChar char
    | char `elem` alphabet = ":alphabet-white-" ++ char : ":"
    | otherwise = [char]
    where alphabet = ['a' .. 'z'] ++ ['A' .. 'Z']

formatString :: String -> String
formatString = unwords . map (foldl (\acc x -> acc ++ formatChar x) []) . words