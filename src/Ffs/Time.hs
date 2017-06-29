module Ffs.Time ( weekForDay ) where

import Data.Tuple.Select
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate as WeekDate

weekForDay :: Day -> (Day, Day)
weekForDay day = (rangeStart, rangeEnd)
  where
    dayOfWeek = toInteger $ sel3 $ toWeekDate day
    rangeStart = addDays (-(dayOfWeek-1)) day
    rangeEnd = addDays (7 - dayOfWeek) day