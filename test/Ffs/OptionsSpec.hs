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

  it "Must not parse the issue option with trailing characters" $ do
    readMaybe "issuesareforsuckers" `shouldBe` (Nothing :: Maybe Grouping)

  it "Must parse the custom field option" $ do
    readMaybe "field:narf" `shouldBe` (Just $ Field "narf")

  it "Must handle custom fields with spaces" $ do
    readMaybe "field:narf zort troz" `shouldBe` (Just $ Field "narf zort troz")

  it "Must trim whitespace from custom fields" $ do
    readMaybe "field: bananas\t" `shouldBe` (Just $ Field "bananas")