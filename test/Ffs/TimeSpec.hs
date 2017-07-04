
module Ffs.TimeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Data.Time.Calendar

import Ffs.Time

spec :: Spec
spec = --describe "Tests" $ do
  intervalGenerator

instance Arbitrary Day where
  arbitrary = do
    day <- arbitrary
    return $ ModifiedJulianDay day

instance Arbitrary DayOfWeek where
  arbitrary = genericArbitrary


intervalGenerator :: Spec
intervalGenerator = describe "Interval generation" $ do
  it "Must generate a week-long range from a single day" $ do
    let d = fromGregorian 1944 6 6
    let (start, end) = weekForDay d Sunday
    start `shouldBe` (fromGregorian 1944 6 5)
    end `shouldBe` (fromGregorian 1944 6 11)

  it "Must generate the following week when seeded with a Monday" $ do
    let mon = fromGregorian 2017 6 26
    let (start, end) = weekForDay mon Sunday
    start `shouldBe` mon
    end `shouldBe` (fromGregorian 2017 7 2)

  it "Must generate the preceeding week when seeded with a Sunday" $ do
    let sun = fromGregorian 2017 7 2
    let (start, end) = weekForDay sun Sunday
    start `shouldBe` (fromGregorian 2017 6 26)
    end `shouldBe` sun

  it "Must handle weeks not ending on a Sunday" $ do
    let d = fromGregorian 1944 6 6  -- D-Day was a Tuesday
    let (start, end) = weekForDay d Friday
    start `shouldBe` (fromGregorian 1944 6 3)
    end `shouldBe` (fromGregorian 1944 6 9)

  it "Must handle weeks ending on days lower than target day" $ do
      let d = fromGregorian 2017 08 04              -- a Friday
      let (start, end) = weekForDay d Tuesday
      start `shouldBe` (fromGregorian 2017 08 02)   -- Week should start in the day after
      end `shouldBe` (fromGregorian 2017 08 08)

  it "Must handle weeks ending on days lower than target day" $ do
      let d = fromGregorian 2017 08 05 -- a Thursday
      let (start, end) = weekForDay d Wednesday
      start `shouldBe` (fromGregorian 2017 08 03)
      end `shouldBe` (fromGregorian 2017 08 09)

  prop "Must always include the supplied date in the generated date range" $
    \(day, endOfWeek) ->
        let (start, end) = weekForDay day endOfWeek
        in (start <= day) && (day <= end)

  prop "Range must always be 7 days" $
      \(day, endOfWeek) ->
          let (start, end) = weekForDay day endOfWeek
          in (end `diffDays` start) == 6

