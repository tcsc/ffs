{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Ffs.Jira  where

import Control.Lens
import qualified Data.ByteString as BS
import Data.Text as Text
import Data.Text.Encoding
import Data.Aeson
import Data.Aeson.Types
import qualified Data.URLEncoded as URLEncoded
import GHC.Generics

import Network.URI
import Network.Wreq as Wreq

import System.Log.Logger as Log
import Text.Printf

info = Log.infoM "jira"
debug = Log.debugM "jira"


data SearchResult = SearchResult
  { _key :: Text
  , _url :: Text
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

data Config = Config
  { username :: Text
  , password :: Text
  , host :: URI
  } deriving (Show)

restPath = "/rest/api/2/search"

escape :: String -> String
escape = escapeURIString isAllowedInURI

searchURI :: URI -> Text -> URI
searchURI host userName =
  host { uriPath = restPath
       , uriQuery = query
       }
  where
     jql :: String
     jql = printf "worklogAuthor=%s and worklogDate >= startOfWeek(-1d) AND worklogDate <= endOfWeek(-1d)" userName

     query :: String
     query = escape $ "?jql=" ++ jql



search :: Wreq.Options -> Config -> Text -> IO (Maybe SearchResults)
search options cfg target = do
  debug $ "cfg: " ++ (show cfg)
  debug $ "AbsUrl: " ++ (show absoluteUrl)
  debug $ "Querying URL: " ++ (show url)
  resp <- Wreq.getWith options url >>= asJSON
  return $ resp ^. responseBody
  where
    url = (uriToString id absoluteUrl) ""
    absoluteUrl = searchURI (host cfg) target