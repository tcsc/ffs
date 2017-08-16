module Ffs.Time
  ( weekForDay
  , DayOfWeek(..)
  , formatDay
  , getTimeInZone
  ) where

import Data.Text
import Data.Tuple.Select
import Data.Time.Clock (getCurrentTime)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate as WeekDate
import Data.Time.Format
import Data.Time.LocalTime (TimeZone(..), ZonedTime(..), utcToZonedTime)
import Text.ParserCombinators.ReadPrec hiding (choice)
import Text.ParserCombinators.ReadP as ReadP
import Text.Read hiding (choice)

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Bounded, Enum, Show, Eq)

instance Read DayOfWeek where
  readPrec = lift $ choice [mon, tue, wed, thu, fri, sat, sun]
    where
    parseDay text val = ReadP.string text >> return val
    mon = parseDay "mon" Monday
    tue = parseDay "tue" Tuesday
    wed = parseDay "wed" Wednesday
    thu = parseDay "thu" Thursday
    fri = parseDay "fri" Friday
    sat = parseDay "sat" Saturday
    sun = parseDay "sun" Sunday

-- | Generates an inclusive date range representing a week that includes a given day
weekForDay :: Day -> DayOfWeek -> (Day, Day)
weekForDay day weekEndsOn = (rangeStart, rangeEnd)
  where
    dayOfWeek = toInteger (sel3 $ toWeekDate day) - 1
    endOfWeek = iso8601Day weekEndsOn - 1
    weekEndIdx =
      if endOfWeek >= dayOfWeek
        then endOfWeek
        else 7 + endOfWeek
    weekEndOffset = weekEndIdx - dayOfWeek
    rangeEnd = addDays weekEndOffset day
    rangeStart = addDays (-6) rangeEnd

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%F"

-- | Translates a DayOfWeek value to it's ISO 8601 WeekDate equivalent
iso8601Day :: DayOfWeek -> Integer
iso8601Day day =
  case day of
    Monday -> 1
    Tuesday -> 2
    Wednesday -> 3
    Thursday -> 4
    Friday -> 5
    Saturday -> 6
    Sunday -> 7


getTimeInZone :: TimeZone -> IO ZonedTime
getTimeInZone tz = utcToZonedTime tz <$> getCurrentTime