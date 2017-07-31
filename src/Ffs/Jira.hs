{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Ffs.Jira
  ( SearchResult(..)
  , issueKey
  , issueURI
  , issueFields
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
  , FieldDescription(..)
  , FieldType(..)
  , fldName
  , fldId
  , fldType
  , fldClauseNames
  , search
  , getWorkLog
  , getFields
  ) where

import Control.Exception
import Control.Lens
import Data.Aeson as Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.HashMap.Lazy as HashMap
import Data.Map.Strict as Map
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
  , _issueFields :: Map Text Aeson.Value
  } deriving (Show, Eq)

makeLenses ''SearchResult

instance FromJSON SearchResult where
  parseJSON = withObject "Search Result" $ \obj -> do
    key <- obj .: "key"
    uri <- obj .: "self"
    fields <- case HashMap.lookup "fields" obj of
                Nothing -> fail $ "key fields not present"
                Just val -> withObject "fields" repack val
    return $ SearchResult key uri fields
    where
      repack :: Aeson.Object -> Parser (Map.Map Text Aeson.Value)
      repack obj = return $
        HashMap.foldlWithKey' (\a k v -> Map.insert k v a) Map.empty obj

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

search :: Wreq.Options -> URI -> Text -> IO [SearchResult]
search options host jql = do
  debug $ printf "Fetching %s..." url
  resp <- Wreq.getWith options url >>= asJSON
  return $ resp ^. responseBody ^. issues
  where
    url = (uriToString id completeUrl) ""
    completeUrl = host
      { uriPath = "/rest/api/2/search"
      , uriQuery = printf "?jql=%s" (escape $ Text.unpack jql)
      }

getWorkLog :: Wreq.Options -> URI -> Text -> IO [WorkLogItem]
getWorkLog options host key = do
  debug $ printf "Starting fetch of log for %s..." key
  response <- Wreq.getWith options url >>= asJSON
  return $ response ^. responseBody ^. logItems
  where
    url = (uriToString id absoluteUrl) ""
    absoluteUrl = host
      { uriPath = printf "/rest/api/2/issue/%s/worklog" key
      }

data FieldType = StringField
               | NumberField
               | ArrayField
               | OptionField
               | DateTimeField
               | AnyField
               | UnknownFieldType
               | OtherFieldType Text
  deriving (Show, Eq)

instance FromJSON FieldType where
  parseJSON = withText "field type" $ \t ->
    let ft = case t of
              "string" -> StringField
              "number" -> NumberField
              "array" -> ArrayField
              "option" -> OptionField
              "any" -> AnyField
              _ -> OtherFieldType t
    in return ft

data FieldDescription = FieldDescription
  { _fldId :: Text
  , _fldName :: Text
  , _fldClauseNames :: [Text]
  , _fldType :: FieldType
  } deriving (Show, Eq)
makeLenses ''FieldDescription

instance FromJSON FieldDescription where
  parseJSON =
    withObject "field description" $ \obj -> do
      fieldId <- obj .: "id"
      fieldName <- obj .: "name"
      clauseNames <- obj .: "clauseNames"
      schema <- obj .:? "schema"
      fieldType <- maybe (return UnknownFieldType) (.: "type") schema
      return $ FieldDescription fieldId fieldName clauseNames fieldType

getFields :: Wreq.Options -> URI -> IO [FieldDescription]
getFields options host = do
  debug $ printf "Fetching field descriptors from %s..." url
  response <- Wreq.getWith options url >>= asJSON
  return $ response ^. responseBody
  where
    url = (uriToString id absoluteUrl) ""
    absoluteUrl = host
      { uriPath = "/rest/api/2/field"
      }