module FfsSpec (spec) where

import Prelude hiding (log)
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Control.Lens
import Network.URI
import Test.Hspec

import Ffs
import Ffs.Args as Args
import Ffs.ConfigFile as Cfg
import Ffs.Jira as Jira
import Ffs.Time as Time

spec :: Spec
spec = describe "Top level tests" $ do
  workLogFilterSpec
  configMergeSpec

defaultUser = Jira.User {
    _userName = "default"
  , _userDisplayName = ""
  , _userUrl = ""
  }

date :: String -> ZonedTime
date s = fromJust $ parseTimeM True defaultTimeLocale "%FT%T%Q%z" s

log = Jira.WorkLogItem {
    _logUrl = ""
  , _logComment = ""
  , _logWorkStarted = date "2017-07-23T13:00:00.000+1000"
  , _logAuthor = defaultUser
  , _logTimeSpent = 1
  }

configMergeSpec :: Spec
configMergeSpec = describe "Merging config file & command line args" $ do
  context "Username" $ do
    defaultSpec optUsername ""
    setByConfigSpec cfgLogin optUsername "potato"
    setByCliSpec login optUsername "duck"
    cliOverridesConfigSpec login "duck" cfgLogin "potato" optUsername

  context "Password" $ do
    defaultSpec optPassword ""
    setByConfigSpec cfgPassword optPassword "potato"
    setByCliSpec password optPassword "duck"
    cliOverridesConfigSpec password "duck" cfgPassword "potato" optPassword

  context "Jira Host" $ do
    let cfgUrl = fromJust $ parseURI "http://example.com/config"
    let cliUrl = fromJust $ parseURI "http://cli.com/cli"
    defaultSpec optJiraHost nullURI
    setByConfigSpec cfgHost optJiraHost cfgUrl
    setByCliSpec url optJiraHost cliUrl
    cliOverridesConfigSpec url cliUrl cfgHost cfgUrl optJiraHost

  context "Use insecure TLS" $ do
    defaultSpec optUseInsecureTLS False
    setByConfigSpec cfgInsecure optUseInsecureTLS True
    setByCliSpec insecure optUseInsecureTLS True
    cliOverridesConfigSpec insecure False cfgInsecure True optUseInsecureTLS

  context "Last Day of Week" $ do
    defaultSpec optLastDayOfWeek Sunday
    setByConfigSpec cfgEndOfWeek optLastDayOfWeek Tuesday
    setByCliSpec lastDayOfWeek optLastDayOfWeek Thursday
    cliOverridesConfigSpec lastDayOfWeek Friday cfgEndOfWeek Monday optLastDayOfWeek

  context "Target user" $ do
    defaultSpec optUser ""
    it "Must be settable from the command line" $ do
      let args = emptyArgs & user .~ "Erik the Red"
      let opts = mergeOptions args emptyConfig
      (opts^.optUser) `shouldBe` "Erik the Red"

defaultSpec optLens def =
  it "Must honour defaults" $ do
    let opts = mergeOptions emptyArgs emptyConfig
    (opts^.optLens) `shouldBe` def

setByConfigSpec cfgLens optLens value =
  it "Must be settable from file configuration" $ do
    let config = emptyConfig & cfgLens ?~ value
    let opts = mergeOptions emptyArgs config
    (opts^.optLens) `shouldBe` value

setByCliSpec cliLens optLens value =
  it "Must be settable from the command line" $ do
    let args = emptyArgs & cliLens ?~ value
    let opts = mergeOptions args emptyConfig
    (opts^.optLens) `shouldBe` value

cliOverridesConfigSpec cliLens cliValue cfgLens cfgValue optLens =
  it "Must favour cli value over file config" $ do
    let config = emptyConfig & cfgLens ?~ cfgValue
    let args = emptyArgs & cliLens ?~ cliValue
    let opts = mergeOptions args config
    (opts^.optLens) `shouldBe` cliValue

workLogFilterSpec :: Spec
workLogFilterSpec = describe "Work log filter" $ do
  it "Must exclude work logs by non-targeted users" $ do
    let targetUser = defaultUser { _userName = "target" }
    let nonTargetUser = defaultUser { _userName = "non-target" }

    let workLog = [ log { _logAuthor = nonTargetUser }
                  , log { _logAuthor = targetUser, _logTimeSpent = 1 }
                  , log { _logAuthor = nonTargetUser }
                  , log { _logAuthor = nonTargetUser }
                  , log { _logAuthor = nonTargetUser }
                  , log { _logAuthor = targetUser, _logTimeSpent = 2 }
                  , log { _logAuthor = targetUser, _logTimeSpent = 3 }
                  ]

    let dateRange = ( fromGregorian 2017 07 20 , fromGregorian 2017 07 26)

    filterWorkLog utc dateRange "target" workLog
      `shouldBe` [workLog!!1, workLog!!5, workLog!!6]

  it "Must exclude items outside the target date range" $ do
    -- create a list of work logs, some of which are outside the target range
    let workLog = [ log { _logWorkStarted = date "2017-07-19T23:59:59.999+0000"}
                  , log { _logWorkStarted = date "2017-07-20T00:00:00.000+0000"}
                  , log { _logWorkStarted = date "2017-07-24T12:00:00.000+0000"}
                  , log { _logWorkStarted = date "2017-07-25T09:30:10.190+0000"}
                  , log { _logWorkStarted = date "2017-07-26T23:59:99.999+0000"}
                  , log { _logWorkStarted = date "2017-07-27T23:00:00.000+0000"}
                  ]

    let dateRange = ( fromGregorian 2017 07 20 , fromGregorian 2017 07 26)

    filterWorkLog utc dateRange "default" workLog
      `shouldBe` [workLog!!1, workLog!!2, workLog!!3, workLog!!4]

  it "Must use timezones when interpreting work log timestamps" $ do
        -- create a list of work logs, some of which are outside the target
        -- range (but only if you include the timezone info in the range
        -- calculation.
    let workLog = [ log { _logWorkStarted = date "2017-07-19T23:59:59.999+0000"}
                  , log { _logWorkStarted = date "2017-07-19T23:00:00.000-0100"}
                  , log { _logWorkStarted = date "2017-07-24T12:00:00.000+0000"}
                  , log { _logWorkStarted = date "2017-07-25T09:30:10.190+0000"}
                  , log { _logWorkStarted = date "2017-07-27T05:00:99.999+0600"}
                  , log { _logWorkStarted = date "2017-07-27T23:00:00.000+0000"}
                  ]
    let dateRange = ( fromGregorian 2017 07 20 , fromGregorian 2017 07 26)

    filterWorkLog utc dateRange "default" workLog
      `shouldBe` [workLog!!1, workLog!!2, workLog!!3, workLog!!4]