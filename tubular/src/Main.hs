{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, QuasiQuotes #-}
module Main where

-- generated by cabal at build time
import Paths_tubular (version)

-- main libraries
import Snap
import Snap.Util.FileServe
import Text.Cassius
import Text.Hamlet
-- import Dhall
import Options.Applicative
import System.Posix.Signals

-- supporting libraries
import System.Directory
import Text.Blaze.Html.Renderer.Text

-- base
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Array (listArray)
import Data.Monoid
import Data.TotalMap
import qualified Data.Map

-- internal
import Daemon
import Experiments
import Pf

data CliArgs = CliArgs {
  configFile :: FilePath,
  pidFile :: FilePath,
  daemonize :: Bool,
  processAction :: String
}

parseCliArgs :: Parser CliArgs
parseCliArgs = CliArgs <$>
  strOption (
    long "config" <>
    metavar "path" <>
    help "Path to config file" <>
    showDefault <>
    value "/etc/tubulard/tubulard.cf") <*>
  strOption (
    long "pid-file" <>
    metavar "path" <>
    help "Path to PID file" <>
    showDefault <>
    value "/var/run/tubulard.pid") <*>
  switch (
    long "daemonize" <>
    help "Run in the background") <*>
  argument str (
    metavar "action" <>
    help "Whether to start or stop the daemon" <>
    showDefault <>
    value "start")

runCliParser :: IO CliArgs
runCliParser = execParser (info (parseCliArgs <**> helper) (fullDesc <> progDesc "A totally tubular firewall frontend" <> header "Tubular"))

main :: IO ()
main = do
  -- get command-line options
  cliArgs <- runCliParser
  -- get config file options
  case processAction cliArgs of
    "start" -> do
      -- daemonize
      case daemonize cliArgs of
        True -> do
          runDaemon (pidFile cliArgs) "tubulard" $ do
            -- print debug info
            putStrLn ("Tubular version: (" <> show version <> ")")
            -- serve http
            let config = defaultConfig
            httpServe config snapMain
        False -> do
          -- print debug info
          putStrLn ("Tubular version: (" <> show version <> ")")
          -- serve http
          let config = defaultConfig
          httpServe config snapMain
    "stop" -> do
      let pidFilePath = pidFile cliArgs
      fileExists <- doesPathExist pidFilePath
      case fileExists of
        -- no other instances running
        False -> error (pidFilePath <> " not found")
        True -> do
          -- see if the pid file references an active process
          pidstr <- readFile pidFilePath
          pid <- readIO pidstr
          guard (pid > 1)
          signalled <- catch (signalProcess keyboardTermination pid >> pure True) (\(SomeException _) -> pure False)
          case signalled of
            False -> error ("Process " <> pidstr <> " in " <> pidFilePath <> " not found")
            True -> do
              -- confirm it's gone
              threadDelay (1 * 1000 * 1000)
              signalled' <- catch (signalProcess nullSignal pid >> pure True) (\(SomeException _) -> pure False)
              case signalled' of
                True -> error ("Process " <> pidstr <> " did not stop")
                False -> pure ()
    s -> error ("Unrecognized action " <> s)
  pure ()

snapMain :: Snap ()
snapMain = do
  route [
    ("/", spage indexdoc),
    ("/css/bootstrap.min.css", serveFile "/usr/local/share/tubular/bootstrap.min.css"),
    ("/css/formtest.css", writeLazyText $ renderCss formtestcss),
    ("/js/custom.js", serveFile "/usr/local/share/tubular/custom.js"),
    ("/formtest", spage formtestdoc),
    ("/wireframe", spage wireframedoc),
    ("/:ignore", spage [shamlet|404|])
    ]

-- for shamlet
spage :: Html -> Snap ()
spage body = writeLazyText (renderHtml [shamlet|
  $doctype 5
  <html>
    <head>
      <meta charset="utf-8">
      <link rel="stylesheet" href="/css/bootstrap.min.css">
      <link rel="stylesheet" href="/css/formtest.css">
      <script src="/js/custom.js">
    <body>
      ^{body}
|])

wireframedoc :: Html
wireframedoc = [shamlet|
<nav .navbar .navbar-dark .bg-secondary>
  <a .navbar-brand href="#">Menu
<div .container-fluid>
  <div .row>
    <div .col-7 .p-0>
      <div .container .bg-info>
        <span .badge .badge-secondary>Source Traffic
      <div .container .bg-primary>
        <span .badge .badge-secondary>Pipeline<br>
        stuff from incoming interface properties<br>
        queue rules<br>
        filter rules<br>
        routing<br>
        stuff from outgoing interface properties<br>
        logic for weird interface types<br>
        arp table or something<br>
        resource limits somewhere<br>
        timeouts somewhere<br>
        state tracking somewhere<br>
      <div .container .bg-info>
        <span .badge .badge-secondary>Result Traffic<br>
        for traffic that's resent locally, link to start over with result traffic<br>
    <div .col-3 .bg-warning>
      <span .badge .badge-secondary>Specific Info
    <div .col-2 .bg-danger>
      <span .badge .badge-secondary>General Info
|]

indexdoc :: Html
indexdoc = [shamlet|
<nav .navbar .navbar-dark .bg-secondary>
  <a .navbar-brand href="#">Menu
<div .container-fluid>
  <div .row>
    <div .col-7 .p-0>
      <div .container>
        <span .badge .badge-secondary>Source Traffic
      <div .container>
        <span .badge .badge-secondary>Pipeline
        <pre>
          #{showConfig testRules}
      <div .container>
        <span .badge .badge-secondary>Result Traffic
    <div .col-3 .bg-light>
      <span .badge .badge-secondary>Specific Info
    <div .col-2 .bg-light>
      <span .badge .badge-secondary>General Info
|]

testRules :: PfLines
testRules = PfLines {
  options = Options {
    timeouts = TimeoutTMap (trim (fromPartial 60 Data.Map.empty)),
    rulesetOptimization = Basic,
    optimization = Pf.Default,
    limits = Limits {
      states = 10000,
      frags = 10000,
      srcNodes = 10000,
      tables = 10000,
      tableEntries = 10000
    },
    logInterface = Nothing,
    blockPolicy = Block,
    statePolicy = Floating,
    stateDefaults = StateOptions {
      maxStates = 10000,
      noSync = False,
      timeouts = TimeoutMap Data.Map.empty,
      sloppy = False,
      pflow = False,
      sourceTrack = NoTracking,
      maxSrcNodes = Nothing,
      maxSrcStates = Nothing,
      maxSrcConn = Nothing,
      maxSrcConnRate = Nothing,
      overload = Nothing,
      ifBound = False
    },
    osFingerprintsFile = Nothing,
    skipOn = listArray (1,0) [],
    debugLevel = Debug,
    reassemble = False,
    forceReassemble = False
  },
  rules = [PfRule {
    action = Pass,
    direction = Just In,
    logOpts = Nothing,
    quick = False,
    on = Nothing,
    addressFamily = Nothing,
    protocols = listArray (0,0) [Protocol "tcp"],
    hosts = Just (FromTo HostAny (listArray (1,0) []) (listArray (1,0) []) HostAny (listArray (0,1) [EqualTo (PortNumber 80), EqualTo (PortNumber 443)])),
    filterOpts = Nothing
  }],
  antispoofRules = [],
  queueRules = [QueueRule {
    name = "std",
    parent = Left (InterfaceName "em0"),
    isDefault = False,
    quantum = Nothing,
    qlimit = 50,
    flows = Nothing,
    bandwidth = Just (Bandwidth (90 * 1000 * 1000) Nothing),
    minBandwidth = Nothing,
    maxBandwidth = Nothing
  }],
  tableRules = [],
  includes = []
}

