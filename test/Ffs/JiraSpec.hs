{-# LANGUAGE QuasiQuotes #-}
module Ffs.JiraSpec (spec) where

import Data.Aeson as Aeson
import Data.ByteString
import Data.Maybe
import Data.String.QQ
import Data.Time.ISO8601
import Test.Hspec

import Ffs.Jira as Jira

spec :: Spec
spec = describe "JIRA REST API" $ do
  jsonParsers

jsonParsers :: Spec
jsonParsers = describe "JSON Parsers" $ do
  searchResultParser
  workLogParser

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
        _issuesStartAt = 0
      , _issuesMaxResults = 50
      , _issuesTotal = 1
      , _issues = [
          Jira.SearchResult "HSP-1" "http://www.example.com/jira/rest/api/2/issue/10001"
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
        _issuesStartAt = 0
      , _issuesMaxResults = 50
      , _issuesTotal = 0
      , _issues = []
    }

    Aeson.decodeStrict(text) `shouldBe` (Just expected)

workLogParser :: Spec
workLogParser = describe "Work log parser" $ do
  it "Must parse the example JSON" $ do
    let text = [s|{
        "startAt": 0,
        "maxResults": 1,
        "total": 1,
        "worklogs": [
            {
                "self": "http://www.example.com/jira/rest/api/2/issue/10010/worklog/10000",
                "author": {
                    "self": "http://www.example.com/jira/rest/api/2/user?username=fred",
                    "name": "fred",
                    "displayName": "Fred F. User",
                    "active": false
                },
                "updateAuthor": {
                    "self": "http://www.example.com/jira/rest/api/2/user?username=fred",
                    "name": "fred",
                    "displayName": "Fred F. User",
                    "active": false
                },
                "comment": "I did some work here.",
                "visibility": {
                    "type": "group",
                    "value": "jira-developers"
                },
                "started": "2013-08-23T16:57:35.985+0200",
                "timeSpent": "3h 20m",
                "timeSpentSeconds": 12000,
                "id": "100028"
            }
        ]
    }|]

    let expected = Jira.WorkLogItems {
        _logStartsAt = 0
      , _logMaxResults = 1
      , _logTotal = 1
      , _logItems = [
          Jira.WorkLogItem {
              _logUrl = "http://www.example.com/jira/rest/api/2/issue/10010/worklog/10000"
            , _logComment = "I did some work here."
            , _logWorkStarted = fromJust $ parseISO8601 "2013-08-23T16:57:35.985+0200"
            , _logAuthor = Jira.User {
                _userName = "fred"
              , _userDisplayName = "Fred F. User"
              , _userUrl = "http://www.example.com/jira/rest/api/2/user?username=fred"
              }
            , _logTimeSpent = 12000
            }
        ]
      }

    Aeson.eitherDecode(text) `shouldBe` (Right expected)