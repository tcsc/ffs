{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}

module Ffs.Args where

import Data.Text
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Network.URI
import Control.Lens.TH
import Options.Applicative
import Paths_ffs (version)
import System.Log.Logger as Log
import Text.Printf

import Ffs.Time (DayOfWeek(..))

data Options = Options
  { _login :: Maybe Text
  , _password :: Maybe Text
  , _loglevel :: Log.Priority
  , _url :: Maybe URI
  , _insecure :: Maybe Bool
  , _lastDayOfWeek :: Maybe DayOfWeek
  , _user :: Text
  } deriving (Eq, Show)

makeLenses ''Options

defaultArgs = Options
  { _login = Nothing
  , _password = Nothing
  , _loglevel = Log.INFO
  , _url = Nothing
  , _insecure = Nothing
  , _lastDayOfWeek = Nothing
  , _user = ""
}

options =
  Options
    <$> (optional $ option text (long "login" <>
                                 short 'l' <>
                                 metavar "USERNAME" <>
                                 value "" <>
                                 help "Your JIRA login"))
    <*> (optional $ option text (long "password" <>
                                 short 'p' <>
                                 metavar "PASSWORD" <>
                                 internal <>
                                 help "Your JIRA password. INSECURE."))
    <*> flag Log.INFO Log.DEBUG (long "verbose" <>
                                 short 'v' <>
                                 help "Be more verbose")
    <*> (optional $ option uri (long "jira" <>
                               help "The url of the jira server" <>
                               metavar "URL"))
    <*> (optional $ switch (long "insecure" <>
                            help "Disable TLS cert checking"))
    <*> (optional $ option auto (long "week-ends-on" <>
                                 short 'e' <>
                                 metavar "DAY" <>
                                 help "The last day of the week, as a 3-letter abbreviation (e.g. mon, tue)"))
    <*> argument text (metavar "USERNAME")

parse :: IO Options
parse = execParser opts
  where
    opts :: ParserInfo Options
    opts = info (options <**> helper) (fullDesc <> header desc)
    desc = printf "ffs v%s - pull a timesheet out of JIRA" (showVersion version)

uri :: ReadM URI
uri = maybeReader parseURI

text :: ReadM Text
text = maybeReader $ Just . pack