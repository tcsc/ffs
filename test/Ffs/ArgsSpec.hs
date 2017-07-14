module Ffs.ArgsSpec (spec) where

import Data.Either
import Test.Hspec
import Ffs.Args
import Ffs.Time (DayOfWeek(..))

spec :: Spec
spec = describe "Argument parser" $ do
  it  "No tests here" $ do
    1 `shouldBe` 1