{-# LANGUAGE QuasiQuotes #-}
module Ffs.JiraSpec (spec) where

import Data.Aeson as Aeson
import Data.ByteString
import Data.HashMap.Lazy as HashMap
import Data.Map.Strict as Map
import Data.Maybe
import Data.String.QQ
import Data.Time.LocalTime
import Data.Time.Format
import Test.Hspec

import Ffs.Jira as Jira

spec :: Spec
spec = describe "JIRA REST API" $ do
  jsonParsers

jsonParsers :: Spec
jsonParsers = describe "JSON Parsers" $ do
  fieldDescriptionParser
  searchResultParser
  workLogParser

date :: String -> ZonedTime
date s = fromJust $ parseTimeM True defaultTimeLocale "%FT%T%Q%z" s

searchResultParser :: Spec
searchResultParser = describe "Search result parser" $ do
  it "Must parse the example search result" $ do
    -- Example search result response sourced from the JIRA REST api docs (with
    -- added fields)
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
               "key": "HSP-1",
               "fields": {
                 "description": "Some text goes here"
               }
           }
       ]
    }|]

    let expected = Jira.SearchResults {
        _issuesStartAt = 0
      , _issuesMaxResults = 50
      , _issuesTotal = 1
      , _issues =
        [ Jira.Issue
            "HSP-1"
            "http://www.example.com/jira/rest/api/2/issue/10001"
            (HashMap.fromList
              [ ("description", Aeson.String "Some text goes here")
              ])
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
            , _logWorkStarted = date "2013-08-23T16:57:35.985+0200"
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

fieldDescriptionParser :: Spec
fieldDescriptionParser =
  describe "The field list parser" $ do
    it "Must parse the example JSON" $ do
      let text = [s|[
          {
              "id": "description",
              "name": "Description",
              "custom": false,
              "orderable": true,
              "navigable": true,
              "searchable": true,
              "clauseNames": [
                  "description"
              ],
              "schema": {
                  "type": "string",
                  "system": "description"
              }
          },
          {
              "id": "summary",
              "key": "summary",
              "name": "Summary",
              "custom": false,
              "orderable": true,
              "navigable": true,
              "searchable": true,
              "clauseNames": [
                  "summary"
              ],
              "schema": {
                  "type": "string",
                  "system": "summary"
              }
          }
      ]|]

      let expected = [ FieldDescription "description" "Description" ["description"] StringField
                     , FieldDescription "summary" "Summary" ["summary"] StringField
                     ]

      Aeson.eitherDecode(text) `shouldBe` (Right expected)

    it "Must handle schemaless fields" $ do
      let text = [s|
        { "clauseNames": ["id", "issue", "issuekey", "key"]
        , "custom": false
        , "id": "issuekey"
        , "name": "Key"
        , "navigable": true
        , "orderable": false
        , "searchable": false
        }|]

      let expected = FieldDescription
                       "issuekey"
                       "Key"
                       ["id", "issue", "issuekey", "key"]
                       UnknownFieldType

      Aeson.eitherDecode(text) `shouldBe` Right expected


