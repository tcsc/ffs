module Ffs
    ( main
    ) where

import Control.Exception
import Control.Lens
import Control.Concurrent.ParallelIO
import Data.List as L
import Data.Map.Strict as Map
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
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

  today <- utctDay <$> getCurrentTime
  debug $ "Today: " ++ formatDay today
  let (start, end) = weekForDay today (args^.lastDayOfWeek)
  info $ printf "Week range:  %s - %s:" (formatDay start) (formatDay end)

  info "Fetching issues worked on..."
  resp <- Jira.search wreqCfg (args^.url) (args^.user) (start, end)
  let myIssues = L.foldl' (\m i -> Map.insert (i^.issueKey) i m)
                          Map.empty
                          (resp^.issues)
  let issueKeys = Map.keys myIssues
  info $ "You worked on " ++ show issueKeys

  fetchWorkLog wreqCfg args (start, end) issueKeys

  return ()

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


wreqConfig :: Args.Options -> Wreq.Options
wreqConfig args =
  let tlsSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
      uid = encodeUtf8 (args ^. login)
      pwd = encodeUtf8 (args ^. password)
  in defaults & auth ?~ basicAuth uid pwd
              & manager .~ Left tlsSettings

initLogger :: Log.Priority -> IO ()
initLogger level = do
  handler <- Log.streamHandler stdout Log.DEBUG
  Log.updateGlobalLogger Log.rootLoggerName (update handler)

  where
      update h = Log.setLevel level . Log.setHandlers [h]
