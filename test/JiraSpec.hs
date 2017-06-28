{-# LANGUAGE QuasiQuotes #-}
module JiraSpec (spec) where

import Test.Hspec
import Data.String.QQ
import Data.Aeson as Aeson
import Data.ByteString

import Jira

spec :: Spec
spec = describe "JIRA REST API" $ do
  jsonParsers

jsonParsers :: Spec
jsonParsers = describe "JSON Parsers" $ do
  searchResultParser

searchResultParser :: Spec
searchResultParser = describe "Search result parser" $ do
  it "Must parse the example search result" $ do
    -- Example search result response sourced from the JIRA REST api docs
    let text = [s|{
       "expand": "names,schema",
       "startAt": 0,
       "maxResults": 50,
       "total": 1,
       "issues": [
           {
               "expand": "",
               "id": "10001",
               "self": "http://www.example.com/jira/rest/api/2/issue/10001",
               "key": "HSP-1"
           }
       ]
    }|]

    let expected = Jira.SearchResults {
        startAt = 0
      , maxResults = 50
      , total = 1
      , issues = [
          Jira.SearchResult {
            key = "HSP-1",
            self = "http://www.example.com/jira/rest/api/2/issue/10001"
          }
        ]
      }

    Aeson.decodeStrict(text) `shouldBe` (Just expected)

  it "Must handle an empty issues list" $ do
    let text = [s|{
      "expand": "names,schema",
      "startAt": 0,
      "maxResults": 50,
      "total": 0,
      "issues": []
    }|]

    let expected = Jira.SearchResults {
        startAt = 0
      , maxResults = 50
      , total = 0
      , issues = []
    }

    Aeson.decodeStrict(text) `shouldBe` (Just expected)