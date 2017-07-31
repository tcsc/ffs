{-# LANGUAGE TemplateHaskell #-}

module Ffs.ConfigFile
  ( Config(..)
  , cfgLogin
  , cfgPassword
  , cfgHost
  , cfgEndOfWeek
  , cfgInsecure
  , cfgGroupBy
  , configFilePath
  , parseConfig
  , loadConfig
  , emptyConfig
  ) where

import Control.Exception
import Control.Lens
import Control.Monad (join)
import Control.Monad.Trans.Except
import Data.ConfigFile hiding (info)
import Data.Either
import Data.Maybe
import Data.Text as T
import Network.URI
import System.Directory
import System.FilePath
import System.Log.Logger as Log

import Ffs.Time (DayOfWeek(..))
import Ffs.Options (Grouping(..))

info = Log.infoM "config"
debug = Log.debugM "config"

data Config = Config
  { _cfgLogin :: Maybe Text
  , _cfgPassword :: Maybe Text
  , _cfgHost :: Maybe URI
  , _cfgEndOfWeek :: Maybe DayOfWeek
  , _cfgInsecure :: Maybe Bool
  , _cfgGroupBy :: Maybe Grouping
  } deriving (Show, Eq)

makeLenses ''Config

configFilePath :: IO FilePath
configFilePath = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".ffs"

parseConfig :: String -> Either CPError Config
parseConfig text = do
  cfg <- readstring emptyCP text
  login <- fmap T.pack <$> maybeGet cfg "login" "username"
  pwd <- fmap T.pack <$> maybeGet cfg "login" "password"
  eow <- maybeGet cfg "report" "week-ends-on"
  group <- maybeGet cfg "report" "group-by"
  host <- asUrl <$> maybeGet cfg "JIRA" "host"
  insecure <- maybeGet cfg "JIRA" "insecure"
  return $
    Config
    { _cfgLogin = login
    , _cfgPassword = pwd
    , _cfgHost = host
    , _cfgEndOfWeek = eow
    , _cfgInsecure = insecure
    , _cfgGroupBy = group
    }
  where
    maybeGet :: Get_C a => ConfigParser ->
                           String ->
                           String -> Either CPError (Maybe a)
    maybeGet cfg sec name = do
      if has_option cfg sec name
        then Just <$> get cfg sec name
        else return Nothing

    asUrl :: Maybe String -> Maybe URI
    asUrl text =
      case text of
        Nothing -> Nothing
        Just s -> parseURI s

emptyConfig =
  Config
  { _cfgLogin = Nothing
  , _cfgPassword = Nothing
  , _cfgHost = Nothing
  , _cfgEndOfWeek = Nothing
  , _cfgInsecure = Nothing
  , _cfgGroupBy = Nothing
  }

loadConfig :: FilePath -> IO Config
loadConfig path = catch loadConfig' handleErr
  where
    loadConfig' :: IO Config
    loadConfig' = do
      text <- readFile path
      let result = parseConfig text
      return $ either (\_ -> emptyConfig) id result
    handleErr :: IOException -> IO Config
    handleErr _ = return emptyConfig
