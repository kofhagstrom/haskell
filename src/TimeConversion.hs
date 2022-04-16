import Data.Char (digitToInt)
import Data.List (intercalate, intersperse)

data Clock = AM | PM | Military deriving (Eq)

instance Show Clock where
  show Military = ""
  show AM = "AM"
  show PM = "PM"

data Time = Time Int Int Int Clock deriving (Eq)

instance Show Time where
  show (Time hour minute seconds clock) = intercalate ":" [hh, mm, ss] ++ show clock
    where
      hh = format hour
      mm = format minute
      ss = format seconds
      format int = (if int < 10 then "0" else "") ++ show int

instance Ord Time where
  compare t1 t2 = compare (calcTotalTime t1) (calcTotalTime t2)

calcTotalTime :: Time -> Int
calcTotalTime time = totTime $ convertToMilitary time
  where
    totTime (Time hour minute seconds clock) = hour * 3600 + minute * 60 + seconds

findSplits :: Eq b => b -> [b] -> [Int]
findSplits delimiter = map fst . filter (\x -> snd x == delimiter) . zip [0 ..]

splitTimeString :: String -> (String, String, String, String)
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
getTime (h, m, s, clock) =
  case length clock of
    2 -> case clock of
      "PM" -> Time hour minute seconds PM
      "AM" -> Time hour minute seconds AM
      _ -> error "AM or PM."
    0 -> Time hour minute seconds Military
    _ -> error "Time string incorrectly formatted."
  where
    hour = parseTime h
    minute = parseTime m
    seconds = parseTime s

stringToTime :: String -> Time
stringToTime = getTime . splitTimeString

convertToMilitary :: Time -> Time
convertToMilitary (Time hour minute seconds clock) =
  case clock of
    AM -> case hour of
      12 -> Time 0 minute seconds Military
      _ -> Time hour minute seconds Military
    PM -> case hour of
      12 -> Time 12 minute seconds Military
      _ -> Time (12 + hour) minute seconds Military
    Military -> Time hour minute seconds clock

main :: IO ()
main = do
  time <- getLine
  print . convertToMilitary $ stringToTime time