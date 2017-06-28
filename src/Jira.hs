{-# LANGUAGE DeriveGeneric #-}
module Jira  where

import Data.Text
import Data.Aeson
import GHC.Generics

data SearchResult = SearchResult
  { key :: Text
  , self :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON SearchResult

data SearchResults = SearchResults
  { startAt :: Int
  , maxResults :: Int
  , total :: Int
  , issues :: [ SearchResult ]
  } deriving (Show, Eq, Generic)

instance FromJSON SearchResults
