{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}
module Ffs.Args where

import Data.Text
import Control.Lens.TH
import System.Console.CmdArgs

data Options = Options
  { _login :: String
  , _password :: String
  , _user :: String
  , _url :: String
  }
  deriving (Data, Eq, Show, Typeable)
makeLenses ''Options

options = Options
  { _login = def &= explicit &= name "login" &= typ "USERNAME" &= help "Your JIRA login"
  , _password = def &= explicit &= name "pwd"  &= typ "PASSWORD" &= help "Your JIRA password. Insecure."
  , _user = def &= explicit &= name "user" &= help "The user to query. Defaults to the target user" &= typ "USERNAME"
  , _url = def &= explicit &= name "url" &= help "The url of JIRA server" &= typ "URL"
  }
  &= verbosity
  &= summary "Summary!"
  &= help "Pull a timecard report out of JIRA"

parse :: IO Options
parse = cmdArgs options