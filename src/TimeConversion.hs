import Data.Char (digitToInt)
import Data.List (intercalate, intersperse)

data Clock = AM | PM | Military deriving (Eq)

instance Show Clock where
  show Military = ""
  show AM = "AM"
  show PM = "PM"

data Time = Time Int Int Int Clock deriving (Eq)

instance Show Time where
  show (Time hour minute second clock) = intercalate ":" [hh, mm, ss ++ show clock]
    where
      hh = format hour
      mm = format minute
      ss = format second
      format int = (if int < 10 then "0" else "") ++ show int

instance Ord Time where
  compare t1 t2 = compare (calcsecond t1) (calcsecond t2)
    where
      calcsecond time = nsecond $ convertToMilitary time
      nsecond (Time hour minute second _) = zipWith (*) [3600, 60, 1] [hour, minute, second]

stringToTime :: String -> Time
stringToTime (h1 : h2 : ':' : m1 : m2 : ':' : s1 : s2 : clock) =
  case length clock of
    2 -> case clock of
      "PM" -> Time hour minute second PM
      "AM" -> Time hour minute second AM
      _ -> error "AM or PM."
    0 -> Time hour minute second Military
    _ -> error "Time string incorrectly formatted."
  where
    hour = parseTime [h1, h2]
    minute = parseTime [m1, m2]
    second = parseTime [s1, s2]
    parseTime = calcTime . map digitToInt
      where
        calcTime time = foldl base10 0 $ zip (reverse time) [0 ..]
        base10 = \acc x -> acc + fst x * 10 ^ snd x
stringToTime _ = error "Time format incorrect"

stringToMaybeTime :: String -> Maybe Time
stringToMaybeTime (h1 : h2 : ':' : m1 : m2 : ':' : s1 : s2 : clock) =
  case length clock of
    2 -> case clock of
      "PM" -> Just (Time hour minute second PM)
      "AM" -> Just (Time hour minute second AM)
      _ -> Nothing
    0 -> Just (Time hour minute second Military)
    _ -> Nothing
  where
    hour = parseTime [h1, h2]
    minute = parseTime [m1, m2]
    second = parseTime [s1, s2]
    parseTime = calcTime . map digitToInt
      where
        calcTime time = foldl base10 0 $ zip (reverse time) [0 ..]
        base10 = \acc x -> acc + fst x * 10 ^ snd x
stringToMaybeTime _ = Nothing


convertToMilitary :: Time -> Time
convertToMilitary (Time hour minute second clock) =
  case clock of
    AM -> case hour of
      12 -> Time 0 minute second Military
      _ -> Time hour minute second Military
    PM -> case hour of
      12 -> Time 12 minute second Military
      _ -> Time (12 + hour) minute second Military
    Military -> Time hour minute second clock

main :: IO ()
main = do
  time <- fmap (convertToMilitary . stringToTime) getLine
  print time
