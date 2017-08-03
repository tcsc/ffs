module Ffs.OptionsSpec (spec) where

import Data.Either
import Test.Hspec
import Text.Read
import Ffs.Options
import Ffs.Time (DayOfWeek(..))

spec :: Spec
spec = groupingParser


groupingParser :: Spec
groupingParser = describe "Grouping option parser" $ do
  it "Must parse the issue option" $
    readMaybe "issue" `shouldBe` Just Issue

  it "Must not parse the issue option with trailing characters" $
    readMaybe "issuesareforsuckers" `shouldBe` (Nothing :: Maybe Grouping)

  it "Must parse the custom field option" $
    readMaybe "field:narf" `shouldBe` (Just $ Field "narf")

  it "Must handle custom fields with spaces" $
    readMaybe "field:narf zort troz" `shouldBe` (Just $ Field "narf zort troz")

  it "Must trim whitespace from custom fields" $
    readMaybe "field: bananas\t" `shouldBe` (Just $ Field "bananas")

  it "Must parse the epic option" $
    readMaybe "epic" `shouldBe` Just Epic

  it "Must not parse the epic option with trailing characters" $
    readMaybe "epicsareasoforsuckers" `shouldBe` (Nothing :: Maybe Grouping)