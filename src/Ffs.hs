{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Ffs
    ( main
    , filterWorkLog
    , collateTimeSheet
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
import Data.Set as Set
import Data.Map.Strict as Map
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Version as Ver (showVersion)
import Network.URI
import Network.Connection      (TLSSettings (..))
import Network.Wreq as Wreq
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Paths_ffs (version)
import System.Log.Logger as Log
import qualified System.Log.Handler.Simple as Log
import System.Exit (exitFailure)
import System.IO
import Text.PrettyPrint.Boxes as Box
import Text.Printf

import Ffs.Args as Args
import Ffs.ConfigFile as ConfigFile
import Ffs.Jira as Jira
import Ffs.Time

err = Log.errorM "main"
info = Log.infoM "main"
debug = Log.debugM "main"

type DateRange = (Day, Day)
type Credentials = (Text, Text)
type IssueMap = Map Text SearchResult
type WorkLogMap = Map Text [WorkLogItem]
type TimeSheet = Map (Day, Text) Int

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
main = doMain `catches` [Handler onHttpException,
                         Handler onError,
                         Handler onSomeException]
  where
    doMain :: IO ()
    doMain = do
      cli <- Args.parse
      initLogger (cli^.loglevel)

      if cli^.argShowVersion
        then putStrLn $ Ver.showVersion version
        else do
          hSetBuffering stdout NoBuffering
          main' cli

    exit :: String -> IO ()
    exit msg = do
      err $ printf "Failed: %s" msg
      exitFailure

    onHttpException :: HttpException -> IO ()
    onHttpException e = do
      let msg = case e of
                  HttpExceptionRequest _ content -> (show content)
                  _ -> show e
      exit msg

    onError :: ErrorCall -> IO ()
    onError (ErrorCallWithLocation msg _) = exit msg

    onSomeException :: SomeException -> IO ()
    onSomeException e = exit $ show e

-- | The part that does all the heavy lifting
--
-- TODO: Decouple the program options from the record returned by the command
--       line parser. It's not a good fit for doing double duty when merged
--       into the file-based config.
main' :: Args -> IO ()
main' cli = do
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
  debug $ printf "Week range:  %s - %s:" (formatDay $ fst dateRange)
    (formatDay $ snd dateRange)

  -- force evaluation of the url now, otherwise we it won't know its bad until
  -- we try to use it.
  url <- getUrl cli options

  info "Fetching issues worked on..."
  let query = buildQuery (options^.optUser) dateRange
  issues <- Jira.search wreqCfg url query
  let myIssues = L.foldl' (\m i -> Map.insert (i^.issueKey) i m)
                          Map.empty
                          issues
  let issueKeys = Map.keys myIssues

  -- get the work logs from JIRA and filter out any that aren't in our range
  -- of interest
  log <- Map.map (filterWorkLog localTimeZone dateRange (options^.optUser))
    <$> fetchWorkLog wreqCfg url dateRange issueKeys

  let ts = collateTimeSheet log id

  putStrLn $ renderTimeSheet dateRange ts

  return ()

-- | Renders a timesheet as a table
renderTimeSheet :: DateRange -> TimeSheet -> String
renderTimeSheet (start, end) timeSheet =
  render $ hsep 2 top (bucketAxis : cols ++ [bucketTotals])
  where
    bucketAxis =
      let header = Box.text ""
          total = Box.text "Total"
          items = L.map (Box.text . T.unpack) buckets
      in Box.vcat left (header : items ++ [total])

    -- Extract the list of buckets from the time sheet. Note that the item order
    -- returned here basically defines the table order
    buckets :: [Text]
    buckets = Set.elems $ Map.foldlWithKey extractBucket Set.empty timeSheet

    bucketTotals :: Box
    bucketTotals =
      let header = Box.text "Total"
          bucketTotals = L.map bucketTotal buckets
          items = L.map timeBox bucketTotals
          grandTotal = timeBox (L.foldl' (+) 0 bucketTotals)
      in Box.vcat right $ header : items ++ [grandTotal]

    timeBox :: Int -> Box
    timeBox = Box.text . fmtTime

    -- Extract the bucket name from the time sheet key and inject it into the
    -- set that is accumulating all of the unique bucket names
    extractBucket :: Set Text -> (Day, Text) -> Int -> Set Text
    extractBucket keys (_, k) _ = Set.insert k keys

    -- Total value of all time worked for a given bucket
    bucketTotal :: Text -> Int
    bucketTotal bucket =
      let addTimeIfInBucket = (\(_, b) v acc -> acc + if b == bucket then v else 0)
      in Map.foldrWithKey addTimeIfInBucket 0 timeSheet

    -- Generate the columns for the table
    cols :: [Box]
    cols = L.map renderDay days

    -- Render a single day's worth of time as a column, including a total
    -- in the final row.
    renderDay :: Day -> Box
    renderDay d =
      let header = Box.text $ formatTime defaultTimeLocale "%a %F" d
          items = L.map (fmtBucketDay d) buckets
          total = L.foldl' (\t b -> let i = fromMaybe 0 $ timeForBucket d b
                                    in t + i)
                           0
                           buckets
      in Box.vcat right $ (header : items) ++ [Box.text $ fmtTime total]

    -- Get the time for a bucket, which may be "Nothing"
    timeForBucket :: Day -> Text -> Maybe Int
    timeForBucket day bucket = Map.lookup (day, bucket) timeSheet

    -- Format the time spent on a given bucket on a given day.
    fmtBucketDay :: Day -> Text -> Box
    fmtBucketDay day bucket =
      let value = fromMaybe "" (fmtTime <$> timeForBucket day bucket)
      in Box.text value

    fmtTime :: Int -> String
    fmtTime seconds =
      let hours = ((fromIntegral seconds) / 3600.0) :: Float
      in printf "%f" hours

    days = L.unfoldr genDay start

    genDay :: Day -> Maybe (Day, Day)
    genDay day =
      if day <= end
        then Just (day, addDays 1 day)
        else Nothing

-- | Extracts the JIRA url from the application options and validates it.
-- Explicitly forces the evaluation of the URL to make sure the validation
-- happens at the right time (i.e. during the evaluation of getUrl, rather
-- than on first use).
getUrl :: Args -> FfsOptions -> IO URI
getUrl cli options =
  let u = case options^.optJiraHost of
            u | u == nullURI -> error "No JIRA URL set"
            u | (uriScheme u) /= "https" ->
              if (cli^.argForce)
                then u
                else error "JIRA url is not HTTPS. Override with --force,\
                           \ but be warned that this will send your\
                           \ credentials over the network in plain text."
            u -> u
  in return $! u


-- | A function that can take an issue key and map it to an aggregation group
-- name
type GroupExtractor = Text -> Text

-- | Collates a collection of work logs into a buckets-per-day grid. The
-- bucket name is generated by applying an extraction function to the
--
collateTimeSheet :: WorkLogMap -> GroupExtractor -> TimeSheet
collateTimeSheet workLog groupExtractor =
  Map.foldlWithKey' aggregateWorkLog Map.empty workLog
  where
    aggregateWorkLog :: TimeSheet -> Text -> [WorkLogItem] -> TimeSheet
    aggregateWorkLog ts key logItems =
      let groupKey = groupExtractor key
      in collateLog groupKey logItems ts


collateLog :: Text -> [WorkLogItem] -> TimeSheet -> TimeSheet
collateLog bucketKey logItems timeSheet =
  L.foldl' accumulateTime timeSheet logItems
  where
    -- Updates the time-sheet entries for all work items in a given issue's
    -- log, aggregating them into per-group buckets
    accumulateTime :: TimeSheet -> WorkLogItem -> TimeSheet
    accumulateTime ts logItem =
      let day = localDay . zonedTimeToLocalTime $ logItem^.logWorkStarted
      in Map.alter (incrementTime logItem) (day, bucketKey) ts

    -- Performs the actual accumulation for a single cell in the timesheet
    -- grid
    incrementTime :: WorkLogItem -> Maybe Int -> Maybe Int
    incrementTime log maybeTotal =
      let total = fromMaybe 0 maybeTotal
      in Just $ total + (log^.logTimeSpent)


-- | Attempts to load a config file from the user's home directory. If the file
-- exists and is parseable, the config file settings are merged with those
-- provided by the user on the CLI.
loadOptions :: Args -> IO (FfsOptions)
loadOptions cli = do
  path <- configFilePath
  debug $ printf "Loading config file from %s..." path
  cfg <- loadConfig path
  return $  mergeOptions cli cfg

-- | A lens operator that sets the value referred to in the lens ONLY if the
-- value is not Nothing.
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

-- | Turns character echoing off on StdIn so we can enter passwords less insecurely
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  oldValue <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin oldValue) action


-- | Extracts the user's JIRA credentials from the settings object, and falls
-- back to prompting the user if they're not already set.
--
getCredentials :: FfsOptions -> IO Credentials
getCredentials args = do
  login <- askIf (args^.optUsername) "JIRA login: "
  pwd <- withEcho False $
    askIf (args^.optPassword) "JIRA password: " <* putStrLn ""
  return (login, pwd)
  where
    askIf :: Text -> String -> IO Text
    askIf val text =
      case val of
        "" -> T.pack <$> (putStr text >> getLine)
        t -> return t


-- | Builds a jql query requesting issues worked on by a given user between
-- two dates (inclusive)
--
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


-- | Takes a complete work log, converts the timestamps to local time and then
-- filters out the items we're not interested in, i.e. things our target user
-- didn't work on and are outside or date range.
--
-- Note that the work items returned by this function have all had their
-- timestamps converted to local time, which should make downstream processing
-- a bit simpler.
--
filterWorkLog :: TimeZone -> DateRange -> Text -> [WorkLogItem] -> [WorkLogItem]
filterWorkLog localTimeZone (start, end) username =
  (L.filter p) . (L.map localiseTimeStamp)
  where
    localiseTimeStamp :: WorkLogItem -> WorkLogItem
    localiseTimeStamp item =
      let logTimeStampUTC = zonedTimeToUTC (item^.logWorkStarted)
          logTimeStampLocal = utcToZonedTime localTimeZone logTimeStampUTC
      in item & logWorkStarted .~ logTimeStampLocal

    p :: WorkLogItem -> Bool
    p item =
      let day = (localDay . zonedTimeToLocalTime) (item^.logWorkStarted)
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
