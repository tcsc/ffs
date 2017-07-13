{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Ffs.Jira
  ( SearchResult(..)
  , issueKey
  , issueURI
  , SearchResults(..)
  , issuesStartAt
  , issuesMaxResults
  , issuesTotal
  , issues
  , User(..)
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
import Data.Time.LocalTime
import qualified Data.URLEncoded as URLEncoded

import Network.URI
import Network.Wreq as Wreq

import System.Log.Logger as Log
import Text.Printf

import Ffs.Time

info = Log.infoM "jira"

debug = Log.debugM "jira"

-- | An issue reference returned as a search result
data SearchResult = SearchResult
  { _issueKey :: Text
  , _issueURI :: Text
  } deriving (Show, Eq)

makeLenses ''SearchResult

instance FromJSON SearchResult where
  parseJSON = withObject "Search Result" $ \obj ->
    SearchResult <$> obj .: "key" <*> obj .: "self"

-- | Search results from a JQL query returning a list of issues
data SearchResults = SearchResults
  { _issuesStartAt :: Int
  , _issuesMaxResults :: Int
  , _issuesTotal :: Int
  , _issues :: [SearchResult]
  } deriving (Show, Eq)

makeLenses ''SearchResults

instance FromJSON SearchResults where
  parseJSON = withObject "Search results" $ \obj ->
    SearchResults
      <$> obj .: "startAt"
      <*> obj .: "maxResults"
      <*> obj .: "total"
      <*> obj .: "issues"

-- | A brief description of a JIRA user account
data User = User
  { _userName :: Text
  , _userDisplayName :: Text
  , _userUrl :: Text
  } deriving (Show, Eq)

makeLenses ''User

instance FromJSON User where
  parseJSON = withObject "User" $ \obj ->
    User <$> obj .: "name" <*> obj .: "displayName" <*> obj .: "self"

instance Eq ZonedTime where
  (==) l r =
    if zonedTimeZone l == zonedTimeZone r
      then zonedTimeToLocalTime l == zonedTimeToLocalTime r
      else zonedTimeToUTC l == zonedTimeToUTC r

-- | A single work log entry
data WorkLogItem = WorkLogItem
  { _logUrl :: Text
  , _logComment :: Text
  , _logWorkStarted :: ZonedTime
  , _logAuthor :: User
  , _logTimeSpent :: Int
  } deriving (Show, Eq)

makeLenses ''WorkLogItem

instance FromJSON WorkLogItem where
  parseJSON = withObject "Work log item" $ \obj ->
       WorkLogItem
        <$> obj .: "self"
        <*> obj .: "comment"
        <*> obj .: "started"
        <*> obj .: "author"
        <*> obj .: "timeSpentSeconds"

-- | A collection of work log items. We assume that they all relate to the
-- same issue.
data WorkLogItems = WorkLogItems
  { _logStartsAt :: Int
  , _logMaxResults :: Int
  , _logTotal :: Int
  , _logItems :: [WorkLogItem]
  } deriving (Show, Eq)

makeLenses ''WorkLogItems

instance FromJSON WorkLogItems where
  parseJSON =
    withObject "Work Logs" $ \obj ->
      WorkLogItems <$> obj .: "startAt" <*> obj .: "maxResults" <*>
      obj .: "total" <*>
      obj .: "worklogs"

-- | URL escape a string
escape :: String -> String
escape = escapeURIString isAllowedInURI

search :: Wreq.Options -> URI -> Text -> IO SearchResults
search options host jql = do
  debug $ printf "Fetching %s..." url
  resp <- Wreq.getWith options url >>= asJSON
  return $ resp ^. responseBody
  where
    url = (uriToString id completeUrl) ""
    completeUrl = host
      { uriPath = "/rest/api/2/search"
      , uriQuery = printf "?jql=%s" (escape $ Text.unpack jql)
      }

getWorkLog :: Wreq.Options -> URI -> Text -> IO WorkLogItems
getWorkLog options host key = do
  debug $ printf "Starting fetch of log for %s..." key
  response <- Wreq.getWith options url >>= asJSON
  return $ response ^. responseBody
  where
    url = (uriToString id absoluteUrl) ""
    absoluteUrl = host
      { uriPath = printf "/rest/api/2/issue/%s/worklog" key
      }