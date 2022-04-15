import Data.Char (digitToInt)

data Time
  = AM Int Int Int
  | PM Int Int Int
  | Military Int Int Int
  deriving (Show, Eq)

findSplits :: Eq b => b -> [b] -> [Int]
findSplits delimiter = map fst . filter (\x -> snd x == delimiter) . zip [0 ..]

splitTimeString :: [Char] -> ([Char], [Char], [Char], [Char])
splitTimeString str = (hour, minute, second, ampm)
  where
    getSplits = splitAt . head $ findSplits ':' str
    (hour, a) = getSplits str
    (minute, b) = getSplits (tail a)
    c = tail b
    second = take 2 c
    ampm = drop 2 c

calcTime :: [Int] -> Int
calcTime time = foldl f 0 $ zip (reverse time) [0 ..]
  where
    f = \acc x -> acc + fst x * 10 ^ snd x

parseTime :: String -> Int
parseTime str = calcTime $ map digitToInt str

getTime :: (String, String, String, String) -> Time
getTime (h, m, s, ampm) =
  case length ampm of
    2 -> case ampm of
      "PM" -> PM hour minute seconds
      "AM" -> AM hour minute seconds
      _ -> error "AM or PM."
    0 -> Military hour minute seconds
    _ -> error "Time string incorrectly formatted."
  where
    hour = parseTime h
    minute = parseTime m
    seconds = parseTime s

convertToMilitary :: Time -> Time
convertToMilitary (AM hour minute seconds) =
  case hour of
    12 -> Military 0 minute seconds
    _ -> Military hour minute seconds
convertToMilitary (PM hour minute seconds) =
  case hour of
    12 -> Military 12 minute seconds
    _ -> Military (12 + hour) minute seconds
convertToMilitary time = time

parseTimeFromString :: String -> Time
parseTimeFromString = getTime . splitTimeString

intToStr :: (Ord a, Num a, Show a) => a -> [Char]
intToStr int = (if int < 10 then "0" else "") ++ show int

timeToString :: Time -> String
timeToString (PM hour minute seconds) = intToStr hour ++ ":" ++ intToStr minute ++ ":" ++ intToStr seconds ++ "PM"
timeToString (AM hour minute seconds) = intToStr hour ++ ":" ++ intToStr minute ++ ":" ++ intToStr seconds ++ "AM"
timeToString (Military hour minute seconds) = intToStr hour ++ ":" ++ intToStr minute ++ ":" ++ intToStr seconds

main :: IO ()
main = do
  time <- getLine
  putStrLn . timeToString . convertToMilitary $ parseTimeFromString time