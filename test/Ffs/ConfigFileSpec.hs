{-# LANGUAGE QuasiQuotes #-}
module Ffs.ConfigFileSpec (spec) where

import Data.String.QQ
import Network.URI
import Test.Hspec

import Ffs.ConfigFile
import Ffs.Time

spec :: Spec
spec = describe "ConfigFile parser" $ do
  it "Must parse a valid config file" $ do
    let text = [s|
[login]
username=some-username
password=p4ssw0r|>

[report]
week-ends-on = fri

[JIRA]
host = https://example.com:1234/
insecure = true
|]
    let expected = Config {
        _cfgLogin = Just "some-username"
      , _cfgPassword = Just "p4ssw0r|>"
      , _cfgHost = parseURI "https://example.com:1234/"
      , _cfgEndOfWeek = Just Friday
      , _cfgInsecure = Just True
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
      }
    parseConfig text `shouldBe` Right expected