{-# LANGUAGE TemplateHaskell #-}

module Ffs.CommandLine
  ( Args(..)
  , argLogin
  , argPassword
  , argLoglevel
  , argUrl
  , argInsecure
  , argLastDayOfWeek
  , argUser
  , argShowVersion
  , argForce
  , emptyArgs
  , parse
  ) where

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

-- | Command line arguments ar parsed by optparse
data Args = Args
  { _argLogin :: Maybe Text
  , _argPassword :: Maybe Text
  , _argLoglevel :: Log.Priority
  , _argUrl :: Maybe URI
  , _argInsecure :: Maybe Bool
  , _argLastDayOfWeek :: Maybe DayOfWeek
  , _argUser :: Text
  , _argShowVersion :: Bool
  , _argForce :: Bool
  } deriving (Eq, Show)
makeLenses ''Args

-- | A default, empty set of args
emptyArgs = Args
  { _argLogin = Nothing
  , _argPassword = Nothing
  , _argLoglevel = Log.INFO
  , _argUrl = Nothing
  , _argInsecure = Nothing
  , _argLastDayOfWeek = Nothing
  , _argUser = ""
  , _argShowVersion = False
  , _argForce = False
}

-- |
options = Args
    <$> (optional $ option text (long "login" <>
                                 short 'l' <>
                                 metavar "USERNAME" <>
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
    <*> switch (long "version" <>
                internal <>
                help "display version and exit")
    <*> switch (long "force" <>
                short 'f' <>
                help "Force insecure and/or dangerous behaviour.")

parse :: IO Args
parse = execParser opts
  where
    opts :: ParserInfo Args
    opts = info (options <**> helper) (fullDesc <> header desc)
    desc = printf "ffs v%s - pull a timesheet out of JIRA" (showVersion version)

uri :: ReadM URI
uri = maybeReader parseURI

text :: ReadM Text
text = maybeReader $ Just . pack