module TimeSpec (spec) where

import Test.Hspec
import Data.Time.Calendar

import Time

spec :: Spec
spec = describe "Tests" $ do
  intervalGenerator


intervalGenerator :: Spec
intervalGenerator = describe "Interval generation" $ do
  it "Must generate a week-long range from a single day" $ do
    let d = fromGregorian 1944 6 6
    let (start, end) = weekForDay d
    start `shouldBe` (fromGregorian 1944 6 5)
    end `shouldBe` (fromGregorian 1944 6 11)

  it "Must generate the following week when seeded with a Monday" $ do
    let mon = fromGregorian 2017 6 26
    let (start, end) = weekForDay mon
    start `shouldBe` mon
    end `shouldBe` (fromGregorian 2017 7 2)

  it "Must generate the preceeding week when seeded with a Sunday" $ do
    let sun = fromGregorian 2017 7 2
    let (start, end) = weekForDay sun
    start `shouldBe` (fromGregorian 2017 6 26)
    end `shouldBe` sun