module Ffs
    ( main
    ) where

import System.Log.Logger as Log
import qualified System.Log.Handler.Simple as Log
import System.Console.CmdArgs.Verbosity
import System.IO

import qualified Ffs.Args as Args


main :: IO ()
main = do
  options <- Args.parse
  getVerbosity >>= initLogger
  return ()


initLogger :: Verbosity -> IO ()
initLogger verbosity = do
  handler <- Log.streamHandler stdout Log.DEBUG
  Log.updateGlobalLogger Log.rootLoggerName (update handler)

  where
      update handler = Log.setLevel loglevel . Log.setHandlers [handler]

      loglevel = case verbosity of
        Quiet -> Log.WARNING
        Normal -> Log.INFO
        Loud -> Log.DEBUG