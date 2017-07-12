{-# LANGUAGE DeriveGeneric #-}

module Ffs.Time
  ( weekForDay
  , DayOfWeek(..)
  , formatDay
  ) where

import Data.Text
import Data.Tuple.Select
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate as WeekDate
import Data.Time.Format

import GHC.Generics

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Generic, Show, Eq)

instance Read DayOfWeek where
  readsPrec _ value = tryParse [
      ("mon", Monday)
    , ("tue", Tuesday)
    , ("wed", Wednesday)
    , ("thu", Thursday)
    , ("fri", Friday)
    , ("sat", Saturday)
    , ("sun", Sunday)
    ]
    where
      tryParse attempts =
        case attempts of
          [] -> []
          (attempt, day) : remainder ->
            if (Prelude.take 3 value) == attempt
               then [(day, Prelude.drop 3 value)]
               else tryParse remainder

-- | Generates an inclusive date range representing a week that includes a given day
weekForDay :: Day -> DayOfWeek -> (Day, Day)
weekForDay day weekEndsOn = (rangeStart, rangeEnd)
  where
    dayOfWeek = (toInteger $ sel3 $ toWeekDate day) - 1
    endOfWeek = (iso8601Day weekEndsOn) - 1
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
