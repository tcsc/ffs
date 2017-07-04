module Ffs
    ( main
    ) where

import Control.Lens
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

main :: IO ()
main = do
  options <- Args.parse
  initLogger (options^.loglevel)
  let wreqCfg = wreqConfig options
  today <- utctDay <$> getCurrentTime

  debug $ "Today: " ++ (formatDay today)
  let (start, end) = weekForDay today (options^.lastDayOfWeek)

  info $ printf "Querying week:  %s - %s:" (formatDay start) (formatDay end)

  debug $ "JIRA host: " ++ (show $ options^.url)

  info "Querying JIRA server..."
  resp <- fromJust <$> Jira.search wreqCfg (options^.url) (options^.user) (start, end)

  let myIssues = L.foldl' (\m i -> Map.insert (i^.key) i m) Map.empty (resp^.issues)
  info $ "You worked on " ++ (show $ Map.keys myIssues)

  return ()

wreqConfig :: Args.Options -> Wreq.Options
wreqConfig args =
  let tlsSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
      uid = encodeUtf8 (args ^. login)
      pwd = encodeUtf8 (args ^. password)
  in defaults & auth ?~ basicAuth uid pwd
              & manager .~ Left (tlsSettings)

initLogger :: Log.Priority -> IO ()
initLogger level = do
  handler <- Log.streamHandler stdout Log.DEBUG
  Log.updateGlobalLogger Log.rootLoggerName (update handler)

  where
      update h = Log.setLevel level . Log.setHandlers [h]
