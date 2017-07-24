module Ffs.OptionsSpec (spec) where

import Data.Either
import Test.Hspec
import Text.Read
import Ffs.Options
import Ffs.Time (DayOfWeek(..))

spec :: Spec
spec = do
  groupingParser


groupingParser :: Spec
groupingParser = describe "Grouping option parser" $ do
  it "Must parse the issue option" $ do
    readMaybe "issue" `shouldBe` (Just Issue)

  it "Must parse the custom field option" $ do
    readMaybe "custom-field:narf" `shouldBe` (Just $ CustomField "narf")

  it "Must handle custom fields with spaces" $ do
    readMaybe "custom-field:narf zort troz" `shouldBe` (Just $ CustomField "narf zort troz")