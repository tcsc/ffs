
module Ffs.TimeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Read
import Data.Maybe
import Data.Time.Calendar

import Ffs.Time

spec :: Spec
spec = do
  intervalGenerator
  dayOfWeekParser
  dateRangeParser

instance Arbitrary Day where
  arbitrary = do
    day <- arbitrary
    return $ ModifiedJulianDay day

instance Arbitrary DayOfWeek where
  arbitrary = arbitraryBoundedEnum


intervalGenerator :: Spec
intervalGenerator = describe "Interval generation" $ do
  it "Must generate a week-long range from a single day" $ do
    let d = fromGregorian 1944 6 6
    let DateRange (start, end) = weekForDay d Sunday
    start `shouldBe` fromGregorian 1944 6 5
    end `shouldBe` fromGregorian 1944 6 11

  it "Must generate the following week when seeded with a Monday" $ do
    let mon = fromGregorian 2017 6 26
    let DateRange (start, end) = weekForDay mon Sunday
    start `shouldBe` mon
    end `shouldBe` fromGregorian 2017 7 2

  it "Must generate the preceeding week when seeded with a Sunday" $ do
    let sun = fromGregorian 2017 7 2
    let DateRange (start, end) = weekForDay sun Sunday
    start `shouldBe` fromGregorian 2017 6 26
    end `shouldBe` sun

  it "Must handle weeks not ending on a Sunday" $ do
    let d = fromGregorian 1944 6 6  -- D-Day was a Tuesday
    let DateRange (start, end) = weekForDay d Friday
    start `shouldBe` fromGregorian 1944 6 3
    end `shouldBe` fromGregorian 1944 6 9

  it "Must handle weeks ending on days lower than target day" $ do
      let d = fromGregorian 2017 08 04            -- a Friday
      let DateRange (start, end) = weekForDay d Tuesday
      start `shouldBe` fromGregorian 2017 08 02   -- Week should start in the day after
      end `shouldBe` fromGregorian 2017 08 08

  it "Must handle weeks ending on days lower than target day" $ do
      let d = fromGregorian 2017 08 05 -- a Thursday
      let DateRange (start, end) = weekForDay d Wednesday
      start `shouldBe` fromGregorian 2017 08 03
      end `shouldBe` fromGregorian 2017 08 09

  prop "Must always include the supplied date in the generated date range" $
    \(day, endOfWeek) ->
        let DateRange (start, end) = weekForDay day endOfWeek
        in (start <= day) && (day <= end)

  prop "Range must always be 7 days" $
    \(day, endOfWeek) ->
        let DateRange (start, end) = weekForDay day endOfWeek
        in (end `diffDays` start) == 6

dayOfWeekParser :: Spec
dayOfWeekParser = describe "The day-of-week parser" $ do
  it "Must parse a day TLA" $ do
    let text = ["mon", "tue", "wed", "thu", "fri", "sat", "sun"]
    let days = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
    let result = map read text
    result `shouldBe` days

  it "Must fail on something not a day" $ do
    let result = (fst <$> listToMaybe (reads "narf")) :: Maybe DayOfWeek
    result `shouldBe` Nothing

dateRangeParser :: Spec
dateRangeParser =
  describe "The date range parser" $ do
    let nothing = Nothing :: Maybe DateRange

    it "Must parse a valid date range" $ do
      let start = fromGregorian 2017 07 01
      let end = fromGregorian 2017 08 30
      let expected = DateRange (start, end)
      readMaybe "2017-07-01..2017-08-30" `shouldBe` Just expected

    it "Must fail on garbage" $
      readMaybe "narf" `shouldBe` nothing

    it "Must fail on invalid dates in start day" $ do
      -- invalid day in start
      readMaybe "2017-02-31..2017-05-24" `shouldBe` nothing

      -- invalid month in start
      readMaybe "2017-13-31..2017-05-24" `shouldBe` nothing
      readMaybe "2017-00-31..2017-05-24" `shouldBe` nothing

      -- invalid day in end
      readMaybe "2017-02-28..2017-05-35" `shouldBe` nothing

      -- invalid month in end
      readMaybe "2017-01-31..2017-24-24" `shouldBe` nothing
      readMaybe "2016-07-31..2017-00-24" `shouldBe` nothing

    it "Must fail on ranges where start > end" $
      readMaybe "2017-01-01..2016-01-01" `shouldBe` nothing
