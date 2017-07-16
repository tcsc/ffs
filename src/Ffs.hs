{-# LANGUAGE TemplateHaskell #-}

module Ffs
    ( main
    , filterWorkLog
    , FfsOptions (..)
    , optUsername
    , optPassword
    , optLogLevel
    , optJiraHost
    , optUseInsecureTLS
    , optLastDayOfWeek
    , optUser
    , mergeOptions
    ) where

import Control.Exception
import Control.Lens
import Control.Lens.TH
import Control.Concurrent.ParallelIO
import Control.Monad
import Data.List as L
import Data.Map.Strict as Map
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.URI
import Network.Connection      (TLSSettings (..))
import Network.Wreq as Wreq
import Network.HTTP.Client.TLS (mkManagerSettings)
import System.Log.Logger as Log
import qualified System.Log.Handler.Simple as Log
import System.IO
import Text.Printf

import Ffs.Args as Args
import Ffs.ConfigFile as ConfigFile
import Ffs.Jira as Jira
import Ffs.Time

info = Log.infoM "main"
debug = Log.debugM "main"

type DateRange = (Day, Day)
type Credentials = (Text, Text)

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

defaultOptions = FfsOptions
  { _optUsername = ""
  , _optPassword = ""
  , _optLogLevel = Log.INFO
  , _optJiraHost = nullURI
  , _optUseInsecureTLS = False
  , _optLastDayOfWeek = Sunday
  , _optUser = ""
  }

-- | Top level main. A thin wrapper around main' for trapping and reporting
-- errors.
main :: IO ()
main = catch main' (\e -> do
  let _ = e :: SomeException
  info $ printf "Failed: %s" (show e) )

-- | The part that does all the heavy lifting
--
-- TODO: Decouple the program options from the record returned by the command
--       line parser. It's not a good fit for doing double duty when merged
--       into the file-based config.
main' :: IO ()
main' = do
  hSetBuffering stdout NoBuffering

  -- Parse command line args and set up the logger
  cli <- Args.parse
  initLogger (cli^.loglevel)

  -- Parse the config file and merge settings with the command line args
  options <- loadOptions cli

  -- get the user credentials, asking the user on the command line if unset.
  credentials <- getCredentials options
  let wreqCfg = wreqConfig credentials options

  localTimeZone <- getCurrentTimeZone
  now <- getZonedTime
  debug $ "Now: " ++ (formatTime defaultTimeLocale "%FT%R%Q%z" now)

  let today = localDay (zonedTimeToLocalTime now)
  debug $ "Today: " ++ formatDay today
  let dateRange = weekForDay today (options^.optLastDayOfWeek)
  info $ printf "Week range:  %s - %s:" (formatDay $ fst dateRange)
    (formatDay $ snd dateRange)

  let url = case options^.optJiraHost of
              nullURI -> error "No JIRA URL set"
              u -> u

  --
  info "Fetching issues worked on..."
  let query = buildQuery (options^.optUser) dateRange
  issues <- Jira.search wreqCfg url query
  let myIssues = L.foldl' (\m i -> Map.insert (i^.issueKey) i m)
                          Map.empty
                          issues
  let issueKeys = Map.keys myIssues
  info $ "You worked on " ++ show issueKeys

  -- get the work logs from JIRA and filter out any that aren't in our range
  -- of interest
  log <- Map.map (filterWorkLog localTimeZone dateRange (options^.optUser))
    <$> fetchWorkLog wreqCfg url dateRange issueKeys

  return ()

-- | Attempts to load a config file from the user's home directory. If the file
-- exists and is parseable, the config file settings are merged with those
-- provided by the user on the CLI.
loadOptions :: Args -> IO (FfsOptions)
loadOptions cli = do
  path <- configFilePath
  debug $ printf "Loading config file from %s..." path
  mergeOptions cli <$> loadConfig path

(~?) :: ASetter' s a -> Maybe a -> s -> s
s ~? Just a  = s .~ a
s ~? Nothing = id

-- | Merges the command-line options with those loaded from file. Produces a
-- new Args.Options with file-based settings overridden by command line args.
mergeOptions :: Args -> Config -> FfsOptions
mergeOptions cmdline file =
  defaultOptions & optUsername ~? (file^.cfgLogin)
                 & optUsername ~? (cmdline^.login)
                 & optPassword ~? (file^.cfgPassword)
                 & optPassword ~? (cmdline^.password)
                 & optJiraHost ~? (file^.cfgHost)
                 & optJiraHost ~? (cmdline^.url)
                 & optUseInsecureTLS ~? (file^.cfgInsecure)
                 & optUseInsecureTLS ~? (cmdline^.insecure)
                 & optLastDayOfWeek ~? (file^.cfgEndOfWeek)
                 & optLastDayOfWeek ~? (cmdline^.lastDayOfWeek)
                 & optUser .~ (cmdline^.user)
--
-- | Turns character echoing off on StdIn so we can enter passwords less insecurely
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  oldValue <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin oldValue) action

-- | Extracts the user's JIRA credentials from the settings object, and falls
-- back to prompting the user if they're not already set
getCredentials :: FfsOptions -> IO Credentials
getCredentials args = do
  login <- askIf (args^.optUsername) "JIRA login: "
  pwd <- withEcho False $ askIf (args^.optPassword) "JIRA password: "
  return (login, pwd)
  where
    askIf :: Text -> String -> IO Text
    askIf val text =
      case val of
        "" -> T.pack <$> (putStr text >> getLine)
        t -> return t


-- | Builds a jql query requesting issues worked on by a given user between
-- two dates (inclusive)
buildQuery :: Text -> DateRange -> Text
buildQuery user (start, end) =
  let s = formatDay start
      e = formatDay end
      jql = printf "worklogAuthor = %s AND worklogDate >= %s AND worklogDate <= %s"
        user s e
  in T.pack jql

-- | Fetches the work logs for all the tickets in the keys list, potentially
-- in parallel.
--
fetchWorkLog :: Wreq.Options ->
                URI ->
                DateRange ->
                [Text] -> IO (Map Text [WorkLogItem])
fetchWorkLog cfg url (start, end) keys = do
  info "Fetching work logs..."
  let tasks = L.map getLog keys
  Map.fromList <$> parallel tasks
  where
    getLog :: Text -> IO (Text, [WorkLogItem])
    getLog k = do
      r <- Jira.getWorkLog cfg url k
      return (k, r)

-- | Takes a complete work log and filters out the items we're not interested
-- in, i.e. things our target user didn't work on and are outside or date
-- range.
--
-- Note that we convert the log timestamp into our local timezone before
-- comparing.
--
filterWorkLog :: TimeZone -> DateRange -> Text -> [WorkLogItem] -> [WorkLogItem]
filterWorkLog localTimeZone (start, end) username = L.filter p
  where
    p :: WorkLogItem -> Bool
    p item =
      -- convert worklog date to local time via UTC
      let logTimeStampUTC = zonedTimeToUTC (item^.logWorkStarted)
          day = localDay $ utcToLocalTime localTimeZone logTimeStampUTC
      in ((item^.logAuthor^.userName) == username) &&
         (day >= start) &&
         (day <= end)

-- | Generates a Wreq configuration from the application options.
wreqConfig :: Credentials -> FfsOptions -> Wreq.Options
wreqConfig (username, password) args =
  let insecureTlsSettings = TLSSettingsSimple True False False
      managerSettings = mkManagerSettings insecureTlsSettings Nothing
      uid = encodeUtf8 username
      pwd = encodeUtf8 password
      opts = defaults & auth ?~ basicAuth uid pwd
  in if args^.optUseInsecureTLS
    then opts & manager .~ Left managerSettings
    else opts


-- | Initialises the haskell logging system based on the verbosity level set
-- by the user.
initLogger :: Log.Priority -> IO ()
initLogger level = do
  handler <- Log.streamHandler stdout Log.DEBUG
  Log.updateGlobalLogger Log.rootLoggerName (update handler)
  where
      update h = Log.setLevel level . Log.setHandlers [h]
