{-# LANGUAGE QuasiQuotes #-}
module Ffs.ConfigFileSpec (spec) where

import Data.String.QQ
import Data.Time.LocalTime
import Network.URI
import Test.Hspec

import Ffs.ConfigFile
import Ffs.Options (Grouping(..))
import Ffs.Time

spec :: Spec
spec = describe "ConfigFile parser" $ do
  it "Must parse a valid config file" $ do
    let text = [s|
[login]
username = some-username
password=p4ssw0r|>

[report]
week-ends-on = fri
group-by = field: narf
roll-up-subtasks = false
target-user = narf-zort-troz

[JIRA]
host = https://example.com:1234/
insecure = true
timezone = -0123
|]
    let expected = Config {
        _cfgLogin = Just "some-username"
      , _cfgPassword = Just "p4ssw0r|>"
      , _cfgHost = parseURI "https://example.com:1234/"
      , _cfgEndOfWeek = Just Friday
      , _cfgInsecure = Just True
      , _cfgGroupBy = Just (Field "narf")
      , _cfgRollUpSubTasks = Just False
      , _cfgTargetUser = Just "narf-zort-troz"
      , _cfgTimeZone = Just $ minutesToTimeZone (-83)
      }
    parseConfig text `shouldBe` Right expected

  it "Must handle an empty file" $ do
    let text = ""
    let expected = Config {
        _cfgLogin = Nothing
      , _cfgPassword = Nothing
      , _cfgHost = Nothing
      , _cfgEndOfWeek = Nothing
      , _cfgInsecure = Nothing
      , _cfgGroupBy = Nothing
      , _cfgRollUpSubTasks = Nothing
      , _cfgTargetUser = Nothing
      , _cfgTimeZone = Nothing
      }
    parseConfig text `shouldBe` Right expected