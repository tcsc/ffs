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
import Data.Time.ISO8601
import Network.URI
import Network.Connection      (TLSSettings (..))
import Network.Wreq as Wreq
import Network.HTTP.Client.TLS (mkManagerSettings)
import System.Log.Logger as Log
import qualified System.Log.Handler.Simple as Log
import System.IO
import Text.Printf

import Ffs.Args as Args
import Ffs.Jira as Jira
import Ffs.Time

info = Log.infoM "main"
debug = Log.debugM "main"

type DateRange = (Day, Day)

main :: IO ()
main = catch main' (\e -> do
  let _ = e :: SomeException
  info "Failed!" )

main' :: IO ()
main' = do
  args <- Args.parse
  initLogger (args^.loglevel)
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
-- in, i.e. things our target user didn't work on and are outside or date range
--
-- Where this gets a bit hairy is that our date range is in local time and the
-- work log items' timestamps are in UTC. This means that we can't simply compare
-- the UTC day for each and be done; first of all we need to convert the UTC
-- timestamp to the local time *at the time of the work log* and compare the
-- resulting local day.
--
-- We also can't simply use the current local timezone, because DST might have
-- kicked in or finished during our reporting interval.
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
