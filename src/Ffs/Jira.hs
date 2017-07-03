{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Ffs.Jira
  ( SearchResult (..)
  , key
  , self
  , SearchResults (..)
  , startAt
  , maxResults
  , total
  , issues
  , search
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.Text as Text
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Format
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
  { _key :: Text
  , _self :: Text
  } deriving (Show, Eq)

makeLenses ''SearchResult

instance FromJSON SearchResult where
  parseJSON (Object obj) = SearchResult <$>
    obj .: "key" <*>
    obj .: "self"
  parseJSON invalid = typeMismatch "Search Results" invalid

data SearchResults = SearchResults
  { _startAt :: Int
  , _maxResults :: Int
  , _total :: Int
  , _issues :: [ SearchResult ]
  } deriving (Show, Eq)


makeLenses ''SearchResults

instance FromJSON SearchResults where
  parseJSON (Object obj) = SearchResults <$>
    obj .: "startAt" <*>
    obj .: "maxResults" <*>
    obj .: "total" <*>
    obj .: "issues"
  parseJSON invalid = typeMismatch "Search Result" invalid

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

search :: Wreq.Options -> URI -> Text -> (Day, Day) -> IO (Maybe SearchResults)
search options host target dateRange = do
  debug $ "AbsUrl: " ++ (show absoluteUrl)
  debug $ "Querying URL: " ++ (show url)
  resp <- Wreq.getWith options url >>= asJSON
  return $ resp ^. responseBody
  where
    url = (uriToString id absoluteUrl) ""
    absoluteUrl = searchURI host target dateRange