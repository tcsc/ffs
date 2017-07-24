{-# LANGUAGE TemplateHaskell #-}

module Ffs.Options where

import Control.Lens
import Control.Lens.TH
import Data.Text
import Network.URI
import System.Log.Logger as Log
import Text.ParserCombinators.ReadPrec hiding (choice)
import Text.ParserCombinators.ReadP as ReadP
import Text.Read hiding (choice)

import Ffs.Time

data Grouping
  = Issue
  | CustomField Text
  deriving (Eq, Show)

instance Read Grouping where
  readPrec = lift $ choice [parseIssue, parseGroup]
    where
      parseIssue = ReadP.string "issue" >> return Issue
      parseGroup = do
        ReadP.string "custom-field:"
        name <- ReadP.munch (\_ -> True)
        return $! CustomField (pack name)


data FfsOptions = FfsOptions
  { _optUsername :: Text
  , _optPassword :: Text
  , _optLogLevel :: Log.Priority
  , _optJiraHost :: URI
  , _optUseInsecureTLS :: Bool
  , _optLastDayOfWeek :: DayOfWeek
  , _optUser :: Text
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
  , _optUser = ""
  }
