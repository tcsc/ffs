{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}
module Ffs.Args where

import Data.Text
import Data.Semigroup ((<>))
import Network.URI
import Control.Lens.TH
import Options.Applicative
import System.Log.Logger as Log

data Options = Options
  { _login :: Text
  , _password :: Text
  , _loglevel :: Log.Priority
  , _url :: URI
  , _insecure :: Bool
  , _user :: Text
  }
  deriving (Eq, Show)
makeLenses ''Options

options = Options
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
  <*> option uri
    ( long "jira" <>
      help "The url of the jira server" <>
      metavar "URL")
  <*> switch (long "insecure" <> help "Disable TLS cert checking")
  <*> argument text (metavar "USERNAME")

parse :: IO Options
parse = execParser opts
  where
    opts :: ParserInfo Options
    opts = info (options <**> helper)
      (fullDesc <> header "Pull a timecard report out of JIRA")

uri :: ReadM URI
uri = maybeReader parseURI

text :: ReadM Text
text = maybeReader $ Just . pack

