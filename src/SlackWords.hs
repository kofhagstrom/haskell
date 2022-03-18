module SlackWords where

f :: String -> String
f = unwords . map (foldl (\acc x -> if x `elem` alphanumerics then acc ++ ":alphabet-white-" ++ [x] ++ ":" else acc ++ [x]) []) . words where alphanumerics = ['a' .. 'z'] ++ ['A' .. 'Z']