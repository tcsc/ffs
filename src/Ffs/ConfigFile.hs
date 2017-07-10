module Ffs.ConfigFile where

import Control.Exception
import Control.Monad (join)
import Control.Monad.Error
import Control.Monad.Trans.Except
import Data.ConfigFile
import Data.Either
import Data.Maybe
import Network.URI
import System.Directory
import System.FilePath
import System.Log.Logger as Log

import Ffs.Args (Options(..))
import Ffs.Time (DayOfWeek(..))

configFilePath :: IO FilePath
configFilePath = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".ffs"

loadConfig :: FilePath -> IO (Maybe Options)
loadConfig path =
  catch (either (\_ -> Nothing) Just <$> runErrorT load)
        (\e -> do let _ = e :: IOException
                  return Nothing)
  where
    load :: ErrorT CPError IO Options
    load = do
      cfg <- join $ liftIO $ readfile emptyCP path
      pwd <- if has_option cfg "DEFAULT" "password"
        then get cfg "DEFAULT" "password"
        else return ""

      jiraHost <- (maybe nullURI id . parseURI) <$>
        if has_option cfg "JIRA" "url"
          then get cfg "JIRA" "url"
          else return ""

      insecure <- if has_option cfg "network" "insecure"
        then get cfg "network" "insecure"
        else return False

      return $ Options {
          _login = ""
        , _password = pwd
        , _loglevel = Log.INFO
        , _url = jiraHost
        , _insecure = insecure
        , _lastDayOfWeek = Friday
        , _user = ""
      }