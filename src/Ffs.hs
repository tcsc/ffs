module Ffs
    ( main
    ) where

import Control.Lens
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Network.URI
import Network.Connection      (TLSSettings (..))
import Network.Wreq as Wreq
import Network.HTTP.Client.TLS (mkManagerSettings)
import System.Log.Logger as Log
import qualified System.Log.Handler.Simple as Log
import System.IO
import Text.Printf

import Ffs.Args as Args
import qualified Ffs.Jira as Jira

info = Log.infoM "main"
debug = Log.debugM "main"

main :: IO ()
main = do
  options <- Args.parse
  initLogger (options^.loglevel)
  let jiraCfg = jiraConfig options
  let wreqCfg = wreqConfig options

  info $ "JIRA host is: " ++ (show $ options^.url)

  info "Querying JIRA server..."
  resp <- Jira.search wreqCfg jiraCfg (options^.user)

  info $ printf "Response: " ++ (show resp)
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
      update handler = Log.setLevel level . Log.setHandlers [handler]

jiraConfig :: Args.Options -> Jira.Config
jiraConfig options =
  Jira.Config (options^.login) (options^.password) (options^.url)
