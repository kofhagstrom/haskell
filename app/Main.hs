module Main where

main :: IO ()
main = do
  putStrLn "Hello, what is your name?"
  name <- getLine
  (putStrLn . greetUser) name

greetUser :: [Char] -> [Char]
greetUser name = "Hello " ++ name ++ ", I'm Haskell!"

