module Ffs
    ( main
    , filterWorkLog
    ) where

import Control.Exception
import Control.Lens
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

main :: IO ()
main = catch main' (\e -> do
  let _ = e :: SomeException
  info $ "Failed!" ++ show e )

main' :: IO ()
main' = do
  cli <- Args.parse
  initLogger (cli^.loglevel)

  args <- loadOptions cli

  let wreqCfg = wreqConfig args

  localTimeZone <- getCurrentTimeZone
  now <- getZonedTime
  debug $ "Now: " ++ (formatTime defaultTimeLocale "%FT%R%Q%z" now)

  let today = localDay (zonedTimeToLocalTime now)
  debug $ "Today: " ++ formatDay today
  let dateRange = weekForDay today (args^.lastDayOfWeek)
  info $ printf "Week range:  %s - %s:" (formatDay $ fst dateRange)
    (formatDay $ snd dateRange)

  --
  info "Fetching issues worked on..."
  resp <- Jira.search wreqCfg (args^.url) (args^.user) dateRange
  let myIssues = L.foldl' (\m i -> Map.insert (i^.issueKey) i m)
                          Map.empty
                          (resp^.issues)
  let issueKeys = Map.keys myIssues
  info $ "You worked on " ++ show issueKeys

  -- get the work logs from JIRA and filter out any that aren't in our range
  -- of interest
  log <- Map.map (filterWorkLog localTimeZone dateRange (args^.user))
    <$> fetchWorkLog wreqCfg args dateRange issueKeys

  traverseWithKey showIssue log
--  let ls = log ! (L.head issueKeys)
--  mapM (info . showIssue) ls

  return ()

-- | Attempts to load a config file from the user's home directory. If the file
-- exists and is parseable, the config file settings are merged with those
-- provided by the user on the CLI.
loadOptions :: Args.Options -> IO (Args.Options)
loadOptions cli = do
  path <- configFilePath
  debug $ printf "Loading config file from %s..." path
  conf <- loadConfig path
  return $ maybe cli (\cfg -> mergeOptions cfg cli) conf


-- | Merges the command-line options with those loaded from file. Produces a
-- new Args.Options with e file-base settings overridden by command line args.
mergeOptions :: Args.Options -> Args.Options -> Args.Options
mergeOptions file cmdline = Args.Options {
      _login = overrideIf "" (file^.login) (cmdline^.login)
    , _password = overrideIf "" (file^.password) (cmdline^.password)
    , _loglevel = cmdline^.loglevel
    , _url = overrideIf nullURI (file^.url) (cmdline^.url)
    , _insecure = overrideIf False (file^.insecure) (cmdline^.insecure)
    , _lastDayOfWeek = cmdline^.lastDayOfWeek
    , _user = cmdline^.user
    }
  where
    overrideIf :: Eq a => a -> a -> a -> a
    overrideIf null base override =
      if override == null then base else override

showIssue :: Text -> [WorkLogItem] -> IO ()
showIssue key items = do
  info $ printf "%s" key
  mapM (info . showItem) items
  return ()

showItem :: WorkLogItem -> String
showItem i =
  let t = formatTime defaultTimeLocale "%FT%R%Q%z" (i^.logWorkStarted)
      c = i^.logComment
  in printf "\t%s: %s" t c

-- | Fetches the work logs for all the tickets in the keys list, potentially
-- in parallel.
--
fetchWorkLog :: Wreq.Options ->
                Args.Options ->
                DateRange ->
                [Text] -> IO (Map Text WorkLogItems)
fetchWorkLog cfg args (start, end) keys = do
  info "Fetching work logs..."
  let tasks = L.map getLog keys
  Map.fromList <$> parallel tasks
  where
    getLog :: Text -> IO (Text, WorkLogItems)
    getLog k = do
      r <- Jira.getWorkLog cfg (args^.url) k
      return (k, r)

-- | Takes a complete work log and filters out the items we're not interested
-- in, i.e. things our target user didn't work on and are outside or date
-- range.
--
-- Note that we convert the log timestamp into our local timezone before
-- comparing.
--
filterWorkLog :: TimeZone -> DateRange -> Text -> WorkLogItems -> [WorkLogItem]
filterWorkLog localTimeZone (start, end) username log =
  L.filter p (log ^. logItems)
  where
    p :: WorkLogItem -> Bool
    p item =
      -- concert worklog date to local time via UTC
      let logTimeStampUTC = zonedTimeToUTC (item^.logWorkStarted)
          day = localDay $ utcToLocalTime localTimeZone logTimeStampUTC
      in ((item^.logAuthor^.userName) == username) &&
         (day >= start) &&
         (day <= end)

-- | Generates a Wreq configuration from the application options.
wreqConfig :: Args.Options -> Wreq.Options
wreqConfig args =
  let tlsSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
      uid = encodeUtf8 (args ^. login)
      pwd = encodeUtf8 (args ^. password)
  in defaults & auth ?~ basicAuth uid pwd
              & manager .~ Left tlsSettings

-- | Initialises the haskell logging system based on the verbosity level set
-- by the user.
initLogger :: Log.Priority -> IO ()
initLogger level = do
  handler <- Log.streamHandler stdout Log.DEBUG
  Log.updateGlobalLogger Log.rootLoggerName (update handler)

  where
      update h = Log.setLevel level . Log.setHandlers [h]
