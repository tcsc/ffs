{-# LANGUAGE TemplateHaskell #-}

module Ffs.Options where

import Control.Lens
import Control.Lens.TH
import Data.Time.Calendar
import Data.Text
import Data.Time.LocalTime (TimeZone)
import Network.URI
import System.Log.Logger as Log
import System.FilePath
import Text.ParserCombinators.ReadPrec hiding (choice)
import Text.ParserCombinators.ReadP as ReadP
import Text.Read hiding (choice)

import Ffs.Time

data Grouping
  = Issue
  | Field Text
  | Epic
  deriving (Eq, Show)

instance Read Grouping where
  readPrec = lift $ choice [parseIssue, parseGroup, parseEpic]
    where
      parseIssue = do
        ReadP.string "issue"
        ReadP.eof
        return Issue
      parseGroup = do
        ReadP.string "field:"
        name <- ReadP.munch (const True)
        return $! Field $ (strip . pack) name
      parseEpic = do
        ReadP.string "epic"
        ReadP.eof
        return Epic

data FfsOptions = FfsOptions
  { _optUsername :: Text
  , _optPassword :: Text
  , _optLogLevel :: Log.Priority
  , _optJiraHost :: URI
  , _optUseInsecureTLS :: Bool
  , _optLastDayOfWeek :: DayOfWeek
  , _optGroupBy :: Grouping
  , _optTargetUser :: Maybe Text
  , _optRollUpSubTasks :: Bool
  , _optTimeZone :: Maybe TimeZone
  , _optDateRange :: Maybe DateRange
  , _optCsvFile :: Maybe FilePath
  } deriving (Show, Eq)

makeLenses ''FfsOptions

defaultOptions =
  FfsOptions
  { _optUsername = ""
  , _optPassword = ""
  , _optLogLevel = Log.INFO
  , _optJiraHost = nullURI
  , _optUseInsecureTLS = False
  , _optLastDayOfWeek = Sunday
  , _optGroupBy = Issue
  , _optTargetUser = Nothing
  , _optRollUpSubTasks = False
  , _optTimeZone = Nothing
  , _optDateRange = Nothing
  , _optCsvFile = Nothing
  }
