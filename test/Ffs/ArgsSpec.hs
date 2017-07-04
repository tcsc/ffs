module Ffs.ArgsSpec (spec) where

import Data.Either
import Test.Hspec
import Ffs.Args
import Ffs.Time (DayOfWeek(..))

spec :: Spec
spec = describe "Argument parser" $ do
  dayOfWeekParserSpec

dayOfWeekParserSpec :: Spec
dayOfWeekParserSpec = describe "The day-of-week parser" $ do
  it "Must parse a day TLA" $ do
    let text = ["mon", "tue", "wed", "thu", "fri", "sat", "sun"]
    let days = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
    let result = rights $ map (\t -> parseDayOfWeek t) text
    result `shouldBe` days

  it "Must fail on something not a day" $ do
    (parseDayOfWeek "narf") `shouldSatisfy` isLeft