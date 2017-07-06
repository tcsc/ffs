{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Ffs.Jira
  ( SearchResult (..)
  , issueKey
  , issueURI
  , SearchResults (..)
  , issuesStartAt
  , issuesMaxResults
  , issuesTotal
  , issues
  , User (..)
  , userName
  , userDisplayName
  , userUrl
  , WorkLogItems(..)
  , logStartsAt
  , logMaxResults
  , logTotal
  , logItems
  , WorkLogItem(..)
  , logUrl
  , logComment
  , logWorkStarted
  , logAuthor
  , logTimeSpent
  , search
  , getWorkLog
  ) where

import Control.Exception
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.Text as Text
import Data.Time.Calendar
import Data.Time.Clock
import Data.Text.Encoding
import Data.Time.Format
import Data.Time.ISO8601
import qualified Data.URLEncoded as URLEncoded
import GHC.Generics

import Network.URI
import Network.Wreq as Wreq

import System.Log.Logger as Log
import Text.Printf

import Ffs.Time

info = Log.infoM "jira"
debug = Log.debugM "jira"


data SearchResult = SearchResult
  { _issueKey :: Text
  , _issueURI :: Text
  } deriving (Show, Eq)

makeLenses ''SearchResult

instance FromJSON SearchResult where
  parseJSON = withObject "Search Result" $ \obj -> SearchResult
    <$> obj .: "key"
    <*> obj .: "self"

data SearchResults = SearchResults
  { _issuesStartAt :: Int
  , _issuesMaxResults :: Int
  , _issuesTotal :: Int
  , _issues :: [ SearchResult ]
  } deriving (Show, Eq)
makeLenses ''SearchResults

instance FromJSON SearchResults where
  parseJSON = withObject "Search results" $ \obj -> SearchResults
    <$> obj .: "startAt"
    <*> obj .: "maxResults"
    <*> obj .: "total"
    <*> obj .: "issues"

data User = User
  { _userName :: Text
  , _userDisplayName :: Text
  , _userUrl :: Text
  }
  deriving (Show, Eq)
makeLenses ''User

instance FromJSON User where
  parseJSON = withObject "User" $ \obj -> User
    <$> obj .: "name"
    <*> obj .: "displayName"
    <*> obj .: "self"

data WorkLogItem  = WorkLogItem
  { _logUrl :: Text
  , _logComment :: Text
  , _logWorkStarted :: UTCTime
  , _logAuthor :: User
  , _logTimeSpent :: Int
  }
  deriving (Show, Eq)
makeLenses ''WorkLogItem

instance FromJSON WorkLogItem where
  parseJSON = withObject "Work log item" $ \obj -> WorkLogItem
    <$> obj .: "self"
    <*> obj .: "comment"
    <*> obj .: "started"
    <*> obj .: "author"
    <*> obj .: "timeSpentSeconds"

data WorkLogItems = WorkLogItems
  { _logStartsAt :: Int
  , _logMaxResults :: Int
  , _logTotal :: Int
  , _logItems :: [WorkLogItem]
  }
  deriving (Show, Eq)
makeLenses ''WorkLogItems

instance FromJSON WorkLogItems where
  parseJSON = withObject "Work Logs" $ \obj -> WorkLogItems
    <$> obj .: "startAt"
    <*> obj .: "maxResults"
    <*> obj .: "total"
    <*> obj .: "worklogs"

escape :: String -> String
escape = escapeURIString isAllowedInURI

searchURI :: URI -> Text -> (Day, Day) -> URI
searchURI host userName (start, end) =
  host { uriPath = "/rest/api/2/search"
       , uriQuery = query
       }
  where
     jql :: String
     jql = printf "worklogAuthor = %s AND worklogDate >= %s AND worklogDate <= %s" userName startText endText

     query :: String
     query = escape $ "?jql=" ++ jql

     startText :: String
     startText = formatDay start

     endText :: String
     endText = formatDay end

search :: Wreq.Options -> URI -> Text -> (Day, Day) -> IO SearchResults
search options host target dateRange = do
    resp <- Wreq.getWith options url >>= asJSON
    return $ resp ^. responseBody
    where
        url = (uriToString id absoluteUrl) ""
        absoluteUrl = searchURI host target dateRange

getWorkLog :: Wreq.Options -> URI -> Text -> IO WorkLogItems
getWorkLog options host key = do
  info $ printf "Fetching work log for %s..." key
  debug $ printf "Fetching work log from %s" url
  response <- Wreq.getWith options url >>= asJSON
  return $ response^.responseBody

  where
    url = (uriToString id absoluteUrl) ""
    absoluteUrl = host { uriPath = path }
    path = printf "/rest/api/2/issue/%s/worklog" key