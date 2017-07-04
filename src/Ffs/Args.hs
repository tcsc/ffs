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
  { _login :: Text
  , _password :: Text
  , _loglevel :: Log.Priority
  , _url :: URI
  , _insecure :: Bool
  , _lastDayOfWeek :: DayOfWeek
  , _user :: Text
  } deriving (Eq, Show)

makeLenses ''Options

options =
  Options
    <$> option text (long "login" <>
                     short 'l' <>
                     metavar "USERNAME" <>
                     help "Your JIRA login")
    <*> option text (long "password" <>
                     short 'p' <>
                     metavar "PASSWORD" <>
                     help "Your JIRA password. INSECURE.")
    <*> flag Log.INFO Log.DEBUG (long "verbose" <>
                                 short 'v' <>
                                 help "Be more verbose")
    <*> option uri (long "jira" <>
                    help "The url of the jira server" <>
                    metavar "URL")
    <*> switch (long "insecure" <>
                help "Disable TLS cert checking")
    <*> option dayOfWeek (long "week-ends-on" <>
                          short 'e' <>
                          metavar "DAY" <>
                          value Friday <>
                          showDefault <>
                          help "The last day of the week, as a 3-letter abbreviation (e.g. mon, tue)")
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


dayOfWeek :: ReadM DayOfWeek
dayOfWeek = eitherReader parseDayOfWeek

parseDayOfWeek :: String -> Either String DayOfWeek
parseDayOfWeek s =
  case s of
    "mon" -> Right Monday
    "tue" -> Right Tuesday
    "wed" -> Right Wednesday
    "thu" -> Right Thursday
    "fri" -> Right Friday
    "sat" -> Right Saturday
    "sun" -> Right Sunday
    _ -> Left $ "Invalid day string: " ++ s