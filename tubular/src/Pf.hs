{-# LANGUAGE DuplicateRecordFields, RecordWildCards, OverloadedStrings #-}
-- data structures that (mostly) fully specify https://man.openbsd.org/pf.conf
module Pf where

import Data.TotalMap
import Data.Map hiding (map)
import Data.Word
import Data.Array
import Data.Text hiding (map, group)
import Data.String
import Data.Maybe
import Data.Monoid
import Data.Bits
import Numeric

-- UNDOCUMENTED
-- in the grammar, ipspec is completely unused

-- UNDOCUMENTED
-- constraints like "> 1024" are OR'd together
-- for instance, pf implements "port {80, 443}" with one rule for each port

-- UNDOCUMENTED
-- this grammar is missing some things from the
-- "from source port source os source to dest port dest" section:
-- Interface names, interface group names, and self can have modifiers appended:
-- :0         Do not include interface aliases.
-- :broadcast Translates to the interface's broadcast address(es).
-- :network   Translates to the network(s) attached to the interface.
-- :peer      Translates to the point-to-point interface's peer address(es).
-- Ranges of addresses are specified using the ‘-’ operator. For instance: “10.1.1.10 - 10.1.1.12” means all addresses from 10.1.1.10 to 10.1.1.12, hence addresses 10.1.1.10, 10.1.1.11, and 10.1.1.12.
-- Host names may also have the :0 modifier appended to restrict the name resolution to the first of each v4 and v6 address found.

data RulesetOptimizationLevel = None | Basic | Profile

data OptimizationLevel = Default | Normal | HighLatency | Satellite | Aggressive | Conservative

data AddressFamily = Inet | Inet6

data BlockPolicy = Block | Return

data StatePolicy = IfBound | Floating

data DebugLevel = Emergency | Alert | Critical | Error | Warning | Notice | Info | Debug

data Direction = In | Out

data TimeoutType =
  TcpFirst |
  TcpOpening |
  TcpEstablished |
  TcpClosing |
  TcpFinwait |
  TcpClosed |
  UdpFirst |
  UdpSingle |
  UdpMultiple |
  IcmpFirst |
  IcmpError |
  OtherFirst |
  OtherSingle |
  OtherMultiple |
  Frag |
  Interval |
  SrcTrack |
  AdaptiveStart |
  AdaptiveEnd
  deriving (Enum, Bounded, Eq, Ord)

data TimeoutTMap = TimeoutTMap (TMap TimeoutType Word)

data TimeoutMap = TimeoutMap (Map TimeoutType Word)

data Limits = Limits {
  states :: Word,
  frags :: Word,
  srcNodes :: Word,
  tables :: Word,
  tableEntries :: Word
}

data FlagType =
  Fin |
  Syn |
  Rst |
  Push |
  Ack |
  Urg |
  Ece |
  Cwr

data FlagMatch = FlagMatch (Map FlagType Bool)

data BandwidthBurst = BandwidthBurst {
  bitsPerSecond :: Word,
  milliseconds :: Word
}

data Bandwidth = Bandwidth {
  bitsPerSecond :: Word,
  burst :: Maybe BandwidthBurst
}

data IcmpMatch =
  Echorep |
  Unreach (Maybe UnreachCode) |
  Squench |
  Redir (Maybe RedirCode) |
  Althost |
  Echoreq |
  Routeradv (Maybe RouteradvCode) |
  Routersol |
  Timex (Maybe TimexCode) |
  Paramprob (Maybe ParamprobCode) |
  Timereq |
  Timerep |
  Inforeq |
  Inforep |
  Maskreq |
  Maskrep |
  Trace |
  Dataconv |
  Mobredir |
  Ipv6Where |
  Ipv6Here |
  Mobregreq |
  Mobregrep |
  Skip |
  Photuris (Maybe PhoturisCode)

data UnreachCode =
  NetUnr |
  HostUnr |
  ProtoUnr |
  PortUnr |
  Needfrag |
  Srcfail |
  NetUnk |
  HostUnk |
  Isolate |
  NetProhib |
  HostProhib |
  NetTos |
  HostTos |
  FilterProhib |
  HostPreced |
  CutoffPreced

data RedirCode =
  RedirNet |
  RedirHost |
  RedirTosNet |
  RedirTosHost

data RouteradvCode =
  NormalAdv |
  CommonAdv

data TimexCode =
  Transit |
  Reassemb

data ParamprobCode =
  Badhead |
  Optmiss |
  Badlen

data PhoturisCode =
  UnknownInd |
  AuthFail |
  DecryptFail

data Icmp6Match =
  Unreach6 (Maybe Unreach6Code) |
  Toobig6 |
  Timex6 (Maybe Timex6Code) |
  Paramprob6 (Maybe Paramprob6Code) |
  Echoreq6 |
  Echorep6 |
  Groupqry6 |
  Listqry6 |
  Grouprep6 |
  Listenrep6 |
  Groupterm6 |
  Listendone6 |
  Routersol6 |
  Routeradv6 |
  Neighbrsol6 |
  Neighbradv6 |
  Redir6 (Maybe Redir6Code) |
  Routrrenum6 |
  Fqdnreq6 |
  Niqry6 |
  Wrureq6 |
  Fqdnrep6 |
  Nirep6 |
  Wrurep6 |
  Mtraceresp6 |
  Mtrace6

data Unreach6Code =
  NorouteUnr6 |
  AdminUnr6 |
  BeyondUnr6 |
  AddrUnr6 |
  PortUnr6

data Timex6Code =
  Transit6 |
  Reassemb6

-- UNDOCUMENTED
-- according to icmp6(4) this has a third unnamed code numbered 2
-- but it's not defined in static const struct icmpcodeent icmp6_code[] in pfctl_parser.c
-- so it's probably safe to not define it here either
data Paramprob6Code =
  Badhead6 |
  Nxthdr6

data Redir6Code =
  Redironlink6 |
  Redirrouter6

-- the name of an interface such as "en0"
data InterfaceName = InterfaceName String

-- must be 1-15 characters and cannot end in a number
data InterfaceGroup = InterfaceGroup String

data Hostname = Hostname String

-- ipv4 address
data Ipv4 = Ipv4 Word32

-- packed-ish ipv6 address - there is no Word128
data Ipv6 = Ipv6 Word64 Word64

data MaskBits = MaskBits Word8

-- hostnames and interfaces will always be configured to auto-update
-- paraphrased from pf.conf(5):
-- Host name resolution and interface to address translation are done at ruleset load-time.
-- When the address of an interface (or host name) changes, the ruleset must be reloaded.
-- Surrounding it in parentheses means the rule is automatically updated.
data Address =
  InterfaceAddress InterfaceName |
  GroupAddress InterfaceGroup |
  HostnameAddress Hostname |
  Ipv4Address Ipv4 |
  Ipv6Address Ipv6

data PortNumber = PortNumber Word16

-- a limited version of Constraint PortNumber just for translation rules
data PortSpec =
  PortEqualTo PortNumber |
  PortGreaterThan PortNumber |
  PortRangeInclusive PortNumber PortNumber

-- a reminder when looking at some types
type OrArray = Array

data Constraint a =
  EqualTo a |
  NotEqualTo a |
  LessThan a |
  LessOrEqual a |
  GreaterThan a |
  GreaterOrEqual a |
  RangeInclusive a a |
  RangeExclusive a a |
  -- (NotRangeInclusive 0 9 :: Constraint Word) means >= 10
  NotRangeInclusive a a

data OSName = OSName String

data User = UserID Word32 | UserName String

-- this is a group that users are in, not what interfaces are in
-- for instance, "wheel"
data Group = GroupID Word32 | GroupName String

data Action =
  Pass |
  Match |
  -- uses block-policy global option
  BlockDefault |
  BlockDrop |
  BlockReturn |
  -- UNDOCUMENTED
  -- ttl could be made more clear to someone not deeply familiar with IP(v4/v6)
  -- it sets return_ttl on a struct, which is type u_int8_t
  -- this probably means it is https://en.wikipedia.org/wiki/Time_to_live
  -- ttl seems to default to 0? it probably hits scrub min-ttl rules too
  ReturnRst |
  ReturnRstTTL Word8 |
  -- UNDOCUMENTED
  -- pf.conf(5) does not make this very clear, but the source does
  -- in /sbin/pfctl/parse.y these options take icmp codes, which depend on a type code
  -- the code is between 0 and 255, and is combined with the type of returnicmpdefault
  -- returnicmpdefault is (ICMP_UNREACH << 8) | ICMP_UNREACH_PORT
  -- or in haskell, (Unreach PortUnr)
  -- returnicmp6default is (ICMP6_DST_UNREACH << 8) | ICMP6_DST_UNREACH_NOPORT
  -- or in haskell, (Unreach6 PortUnr6)
  ReturnIcmp4 UnreachCode |
  ReturnIcmpBoth UnreachCode Unreach6Code |
  ReturnIcmp6 Unreach6Code

-- for completeness, should probably be removed later
data Include = Include FilePath

-- keeps connections from the same source mapped to the same destionation
-- mappings are destroyed with the last state, or last longer with "set timeout src.track"
data StickyAddress = StickyAddress Bool

-- destination pool for translation rules
data PoolType =
  Bitmask |
  LeastStates StickyAddress |
  Random StickyAddress |
  RoundRobin StickyAddress |
  -- optional hashing key, or randomly initialized at pf startup
  SourceHash (Maybe String)

data StateMode =
  NoState |
  KeepState (Maybe StateOptions) |
  ModulateState (Maybe StateOptions) |
  SynproxyState (Maybe StateOptions)

data StateOptions = StateOptions {
  maxStates :: Word,
  noSync :: Bool,
  timeouts :: TimeoutMap,
  sloppy :: Bool,
  pflow :: Bool,
  sourceTrack :: SourceTrack,
  maxSrcNodes :: Maybe Word,
  maxSrcStates :: Maybe Word,
  maxSrcConn :: Maybe Word,
  -- maxSrcConnRate = (num, secs)
  -- limit rate of new connections in a rolling time window
  maxSrcConnRate :: Maybe (Word, Word),
  overload :: Maybe (Table, FlushMode),
  -- default floating (false)
  ifBound :: Bool
}

data FlushMode =
  NoFlush |
  Flush |
  FlushGlobal

data SourceTrack =
  NoTracking |
  GlobalTracking |
  RuleTracking

-- strings must exist in /etc/protocols
data Protocol = Protocol String

data Table = Table String

data TableAddr =
  TAHostname Hostname |
  TAIfspec (Array Word InterfaceSpec) |
  TASelf |
  TAIpv4 Ipv4 |
  TAIpv6 Ipv6

data TableAddrSpec = TableAddrSpec {
  match :: Bool,
  address :: TableAddr,
  maskBits :: Maybe MaskBits
}

data TableRule = TableRule {
  name :: Table,
  persist :: Bool,
  constFlag :: Bool,
  counters :: Bool,
  sourceFiles :: Array Word FilePath,
  addresses :: Array Word TableAddrSpec
}

data InterfaceSpec =
  InterfaceMatch (Either InterfaceName InterfaceGroup) |
  InterfaceReject (Either InterfaceName InterfaceGroup)

data LogOpts = LogOpts {
  logAll :: Bool,
  logMatches :: Bool,
  logUser :: Bool,
  -- the default interface is pflog0
  logInterface :: InterfaceName
}

-- UNDOCUMENTED
-- the grammar is missing documented cases: critical, inetcontrol, netcontrol
-- Pf calls this TOS for "type of service", but the IPv4 TOS field was replaced with the DS field
data TOS =
  TOSCritical |
  InetControl |
  LowDelay |
  NetControl |
  Throughput |
  Reliability |
  DiffServCodePoint String |
  -- this is actually 6 bits
  -- information on what the numbers mean is hard to find, leave it as a raw number here
  DiffServNumber Word8

-- cannot be 0
data Weight = Weight Word16

data Host =
  MatchAddress Address (Maybe MaskBits) (Maybe Weight) |
  NotMatchAddress Address (Maybe MaskBits) (Maybe Weight) |
  MatchTable Table |
  NotMatchTable Table

data RouteHost =
  RHHost Host (Maybe InterfaceName) |
  RHInterface InterfaceName (Maybe (Address, Maybe MaskBits))

data RouteMethod =
  RouteTo |
  ReplyTo |
  DupTo

data Route = Route {
  method :: RouteMethod,
  destination :: Array Word RouteHost,
  poolType :: Maybe PoolType
}

data Options = Options {
  timeouts :: TimeoutTMap,
  rulesetOptimization :: RulesetOptimizationLevel,
  optimization :: OptimizationLevel,
  limits :: Limits,
  logInterface :: Maybe InterfaceName,
  blockPolicy :: BlockPolicy,
  statePolicy :: StatePolicy,
  stateDefaults :: StateOptions,
  osFingerprintsFile :: Maybe FilePath,
  skipOn :: Array Word InterfaceSpec,
  debugLevel :: DebugLevel,
  reassemble :: Bool,
  forceReassemble :: Bool
}

data AntispoofRule = AntispoofRule {
  shouldLog :: Bool,
  quick :: Bool,
  forInterfaces :: Array Word InterfaceSpec,
  forAddressFamily :: Maybe AddressFamily,
  label :: Maybe String
}

data QueueRule = QueueRule {
  name :: String,
  parent :: Either InterfaceName String,
  isDefault :: Bool,
  quantum :: Maybe Word,
  -- default is 50
  qlimit :: Word,
  -- this only supports values up to maxBound :: Int16
  flows :: Maybe Word16,
  bandwidth :: Maybe Bandwidth,
  minBandwidth :: Maybe Bandwidth,
  maxBandwidth :: Maybe Bandwidth
}

data HostsFromTo =
  HostAny |
  HostNoRoute |
  HostUrpfFailed |
  HostSelf |
  Hosts (Array Word Host) |
  -- the name of a routing table route
  HostRoute String

data Hosts =
  AllHosts |
  -- the second HostsFromTo cannot be urpf-failed
  FromTo HostsFromTo (OrArray Word (Constraint PortNumber)) (OrArray Word OSName) HostsFromTo (OrArray Word (Constraint PortNumber))

data ScrubOpts = ScrubOpts {
  nodf :: Bool,
  -- uses IP number of hops
  minTTL :: Maybe Word,
  maxMSS :: Maybe Word,
  reassembleTCP :: Bool,
  randomID :: Bool
}

-- options to filter rules above and beyond the mandatory
-- "from source port source os source to dest port dest"
data FilterOpts = FilterOpts {
  allowOpts :: Bool,
  divertPort :: OrArray Word (Constraint PortNumber),
  divertReply :: Bool,
  divertTo :: (Host, OrArray Word (Constraint PortNumber)),
  tcpFlags :: FlagMatch,
  group :: OrArray Word (Constraint Group),
  icmpMatch :: Array Word IcmpMatch,
  icmp6Match :: Array Word Icmp6Match,
  label :: Maybe String,
  -- fst packets per snd seconds
  rateLimit :: Maybe (Word, Word),
  once :: Bool,
  -- integer percentage from 0 to 100
  probability :: Word8,
  -- ranges from 0 to 7
  matchPriority :: Maybe Word8,
  -- match can be negated
  receivedOn :: Maybe (Bool, Either InterfaceName InterfaceGroup),
  -- routing tables are determined externally
  rtable :: Maybe Word,
  -- milliseconds
  setDelay :: Word,
  -- ranges from 0 to 7, second priority is for ack and lowdelay packets
  setPriority :: Maybe (Word8, Maybe Word8),
  -- must match QueueRule names, second queue is for ack and lowdelay packets
  setQueue :: Maybe (String, Maybe String),
  setTag :: Maybe String,
  -- match can be negated
  matchTag :: Maybe (Bool, String),
  setTOS :: Maybe TOS,
  matchTOS :: Maybe TOS,
  user :: OrArray Word (Constraint User),
  translateToAF :: Maybe (AddressFamily,
                          Array Word (Address, Maybe MaskBits),
                          Array Word (Address, Maybe MaskBits)),
  -- equivalent to combination of nat and rdr rules
  translateBiNat :: (Array Word (Address, Maybe MaskBits),
                     Maybe PortSpec,
                     Maybe PoolType),
  -- optionally don't modify source port
  translateNat :: (Array Word (Address, Maybe MaskBits),
                   Maybe PortSpec,
                   Maybe PoolType,
                   Bool),
  translateRdr :: (Array Word (Address, Maybe MaskBits),
                   Maybe PortSpec,
                   Maybe PoolType),
  scrub :: ScrubOpts,
  fragment :: Bool,
  state :: StateMode,
  setRoute :: Maybe Route
}

data PfRule = PfRule {
  action :: Action,
  direction :: Maybe Direction,
  logOpts :: Maybe LogOpts,
  quick :: Bool,
  -- either match on target interface or target routing domain
  on :: Maybe (Either (Array Word InterfaceSpec) (Word)),
  addressFamily :: Maybe AddressFamily,
  protocols :: Array Word Protocol,
  hosts :: Maybe Hosts,
  filterOpts :: Maybe FilterOpts
}

data PfLines = PfLines {
  options :: Options,
  rules :: [PfRule],
  antispoofRules :: [AntispoofRule],
  queueRules :: [QueueRule],
  tableRules :: [TableRule],
  includes :: [Include]
}

showConfig :: PfLines -> Text
showConfig (PfLines {..}) = optionsString <> rulesString <> antispoofString <> queueString <> tableString <> includesString
  where
    optionsString = showOptions options <> "\n"
    rulesString = intercalate "\n" (map showRule rules) <> "\n"
    antispoofString = intercalate "\n" (map showAntispoof antispoofRules) <> "\n"
    queueString = intercalate "\n" (map showQueue queueRules) <> "\n"
    tableString = intercalate "\n" (map showTable tableRules) <> "\n"
    includesString = intercalate "\n" (map showInclude includes) <> "\n"

showOptions :: Options -> Text
showOptions (Options {..}) = intercalate "\n" [timeoutsString, rulesetOptimizationString, optimizationString, limitsString, logInterfaceString, blockPolicyString, statePolicyString, stateDefaultsString, osFingerprintsString, skipOnString, debugLevelString, reassembleString]
  where
    timeoutsString = let TimeoutTMap m = timeouts in "set timeout {\n  " <> intercalate ",\n  " (map (\t -> showTimeoutType t <> " " <> pack (show (m Data.TotalMap.! t))) [minBound .. maxBound]) <> "}"
    rulesetOptimizationString = "set ruleset-optimization " <> showRulesetOptimization rulesetOptimization
    optimizationString = case optimization of
      Default -> ""
      _ -> "set optimization " <> showOptimizationLevel optimization
    limitsString = showLimits limits
    logInterfaceString = case logInterface of
      Nothing -> "set loginterface none"
      Just (InterfaceName s) -> "set loginterface " <> pack s
    blockPolicyString = "set block-policy " <> showBlockPolicy blockPolicy
    statePolicyString = "set state-policy " <> showStatePolicy statePolicy
    stateDefaultsString = "set state-defaults " <> showStateOpts stateDefaults
    osFingerprintsString = case osFingerprintsFile of
      Nothing -> ""
      Just f -> "set fingerprints \"" <> stringEscape (pack f) <> "\""
    skipOnString = "set skip on {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showInterfaceSpec skipOn)) <> "}"
    debugLevelString = "set debug " <> showDebugLevel debugLevel
    reassembleString = "set reassemble " <> Data.Text.intercalate " " (catMaybes [Just (yesnoKeyword reassemble), boolKeyword forceReassemble "no-df"])

showInclude :: Include -> Text
showInclude (Include path) = Data.Text.concat ["include \"", stringEscape (pack path), "\""]

boolKeyword :: Bool -> a -> Maybe a
boolKeyword True s = Just s
boolKeyword False _ = Nothing

yesnoKeyword :: IsString p => Bool -> p
yesnoKeyword True = "yes"
yesnoKeyword False = "no"

showDebugLevel :: IsString p => DebugLevel -> p
showDebugLevel Emergency = "emerg"
showDebugLevel Alert = "alert"
showDebugLevel Critical = "crit"
showDebugLevel Error = "err"
showDebugLevel Warning = "warning"
showDebugLevel Notice = "notice"
showDebugLevel Info = "info"
showDebugLevel Debug = "debug"

showBlockPolicy :: IsString p => BlockPolicy -> p
showBlockPolicy Block = "block"
showBlockPolicy Return = "return"

showOptimizationLevel :: IsString p => OptimizationLevel -> p
showOptimizationLevel Default = ""
showOptimizationLevel Normal = "normal"
showOptimizationLevel HighLatency = "high-latency"
showOptimizationLevel Satellite = "satellite"
showOptimizationLevel Aggressive = "aggressive"
showOptimizationLevel Conservative = "conservative"

showStatePolicy :: IsString p => StatePolicy -> p
showStatePolicy IfBound = "if-bound"
showStatePolicy Floating = "floating"

showInterfaceSpec :: InterfaceSpec -> Text
showInterfaceSpec (InterfaceMatch e) = showInterface e
showInterfaceSpec (InterfaceReject e) = "!" <> showInterface e

showInterface :: Either InterfaceName InterfaceGroup -> Text
showInterface (Left (InterfaceName s)) = pack s
showInterface (Right (InterfaceGroup s)) = pack s

showLimits :: Limits -> Text
showLimits (Limits {..}) = "set limit {states " <> textShow states <> ", frags " <> textShow frags <> ", src-nodes " <> textShow srcNodes <> ", tables " <> textShow tables <> ", table-entries " <> textShow tableEntries <> "}"

showRulesetOptimization :: IsString p => RulesetOptimizationLevel -> p
showRulesetOptimization None = "none"
showRulesetOptimization Basic = "basic"
showRulesetOptimization Profile = "profile"

showTimeoutType :: IsString p => TimeoutType -> p
showTimeoutType TcpFirst = "tcp.first"
showTimeoutType TcpOpening = "tcp.opening"
showTimeoutType TcpEstablished = "tcp.established"
showTimeoutType TcpClosing = "tcp.closing"
showTimeoutType TcpFinwait = "tcp.finwait"
showTimeoutType TcpClosed = "tcp.closed"
showTimeoutType UdpFirst = "udp.first"
showTimeoutType UdpSingle = "udp.single"
showTimeoutType UdpMultiple = "udp.multiple"
showTimeoutType IcmpFirst = "icmp.first"
showTimeoutType IcmpError = "icmp.error"
showTimeoutType OtherFirst = "other.first"
showTimeoutType OtherSingle = "other.single"
showTimeoutType OtherMultiple = "other.multiple"
showTimeoutType Frag = "frag"
showTimeoutType Interval = "interval"
showTimeoutType SrcTrack = "src.track"
showTimeoutType AdaptiveStart = "adaptive.start"
showTimeoutType AdaptiveEnd = "adaptive.end"

showStateOpts :: StateOptions -> Text
showStateOpts (StateOptions {..}) = intercalate ", " (catMaybes [maxString, noSyncString, timeoutsString, sloppyString, pflowString, sourceTrackString, maxSrcNodesString, maxSrcStatesString, maxSrcConnString, maxSrcConnRateString, overloadString, ifBoundString])
  where
    maxString = Just ("max " <> textShow maxStates)
    noSyncString = if noSync then Just "no-sync" else Nothing
    timeoutsString = let TimeoutMap m = timeouts in if Data.Map.null m then Nothing else Just (intercalate ", " (map (\t -> showTimeoutType t <> " " <> pack (show (m Data.Map.! t))) (keys m)))
    sloppyString = if sloppy then Just "sloppy" else Nothing
    pflowString = if pflow then Just "pflow" else Nothing
    sourceTrackString = case sourceTrack of
      NoTracking -> Nothing
      GlobalTracking -> Just "source-track global"
      RuleTracking -> Just "source-track rule"
    maxSrcNodesString = case maxSrcNodes of
      Nothing -> Nothing
      Just n -> Just ("max-src-nodes " <> textShow n)
    maxSrcStatesString = case maxSrcStates of
      Nothing -> Nothing
      Just n -> Just ("max-src-states " <> textShow n)
    maxSrcConnString = case maxSrcConn of
      Nothing -> Nothing
      Just n -> Just ("max-src-conn " <> textShow n)
    maxSrcConnRateString = case maxSrcConnRate of
      Nothing -> Nothing
      Just (n,r) -> Just ("max-src-conn-rate " <> textShow n <> "/" <> textShow r)
    overloadString = case overload of
      Nothing -> Nothing
      Just ((Table t), f) -> Just ("overload <" <> pack t <> "> " <> showFlushMode f)
    ifBoundString = if ifBound then Just "if-bound" else Just "floating"

showFlushMode :: IsString p => FlushMode -> p
showFlushMode NoFlush = ""
showFlushMode Flush = "flush"
showFlushMode FlushGlobal = "flush global"

showAntispoof :: AntispoofRule -> Text
showAntispoof (AntispoofRule {..}) = "antispoof " <> logString <> quickString <> "for " <> ifspecString <> afString <> labelString
  where
    logString = if shouldLog then "log " else ""
    quickString = if quick then "quick " else ""
    ifspecString = "{" <> intercalate ", " (map showIfspec (Data.Array.elems forInterfaces)) <> "}"
    afString = case forAddressFamily of
      Nothing -> ""
      Just Inet -> "inet"
      Just Inet6 -> "inet6"
    labelString = case label of
      Nothing -> ""
      Just l -> "label " <> pack l

showIfspec :: InterfaceSpec -> Text
showIfspec (InterfaceMatch i) = case i of
  Left (InterfaceName s) -> pack s
  Right (InterfaceGroup s) -> pack s
showIfspec (InterfaceReject i) = "!" <> case i of
  Left (InterfaceName s) -> pack s
  Right (InterfaceGroup s) -> pack s

showQueue :: QueueRule -> Text
showQueue (QueueRule {..}) = "queue " <> pack name <> " " <> parentString <> defaultString <> quantumString <> qlimitString <> flowsString <> bandwidthString <> minBandwidthString <> maxBandwidthString
  where
    parentString = case parent of
      Left (InterfaceName s) -> "on " <> pack s <> " "
      Right label -> "parent " <> pack label <> " "
    defaultString = if isDefault then "default " else ""
    quantumString = case quantum of
      Nothing -> ""
      Just n -> "quantum " <> textShow n <> " "
    qlimitString = "qlimit " <> textShow qlimit <> " "
    flowsString = case flows of
      Nothing -> ""
      Just n -> "flows " <> textShow n <> " "
    bandwidthString = case bandwidth of
      Nothing -> ""
      Just b -> "bandwidth " <> showBandwidth b <> " "
    minBandwidthString = case minBandwidth of
      Nothing -> ""
      Just b -> "min " <> showBandwidth b <> " "
    maxBandwidthString = case maxBandwidth of
      Nothing -> ""
      Just b -> "max " <> showBandwidth b <> " "

showBandwidth :: Bandwidth -> Text
showBandwidth (Bandwidth {..}) = textShow bitsPerSecond <> burstString
  where
    burstString = case burst of
      Nothing -> ""
      Just burst' -> " " <> showBandwidthBurst burst'

showBandwidthBurst :: BandwidthBurst -> Text
showBandwidthBurst (BandwidthBurst {..}) = "burst " <> textShow bitsPerSecond <> " for " <> textShow milliseconds <> " ms"

showTable :: TableRule -> Text
showTable (TableRule {..}) = "table <" <> nameString <> "> " <> persistString <> constString <> countersString <> sourceFilesString <> addressesString
  where
    nameString = case name of Table s -> pack s
    persistString = if persist then "persist " else ""
    constString = if constFlag then "const " else ""
    countersString = if counters then "counters " else ""
    sourceFilesString = intercalate " " (map (\s -> "file " <> pack s) (Data.Array.elems sourceFiles))
    addressesString = "{" <> intercalate ", " (map showTableAddrSpec (Data.Array.elems addresses)) <> "}"

showTableAddrSpec :: TableAddrSpec -> Text
showTableAddrSpec (TableAddrSpec {..}) = matchString <> addressString <> maskString
  where
    matchString = if match then "" else "!"
    addressString = showTableAddr address
    maskString = case maskBits of
      Nothing -> ""
      Just (MaskBits n) -> "/" <> textShow n

showTableAddr :: TableAddr -> Text
showTableAddr (TAHostname (Hostname s)) = pack s
showTableAddr (TAIfspec ifs) = "{" <> intercalate ", " (map showIfspec (Data.Array.elems ifs)) <> "}"
showTableAddr TASelf = "self"
showTableAddr (TAIpv4 addr) = showIpv4 addr
showTableAddr (TAIpv6 addr) = showIpv6 addr

showIpv4 :: Ipv4 -> Text
showIpv4 (Ipv4 n) = oct1 <> "." <> oct2 <> "." <> oct3 <> "." <> oct4
  where
    oct1 = textShow (shiftR n 24 .&. 0xff)
    oct2 = textShow (shiftR n 16 .&. 0xff)
    oct3 = textShow (shiftR n 8 .&. 0xff)
    oct4 = textShow (shiftR n 0 .&. 0xff)

showIpv6 :: Ipv6 -> Text
showIpv6 (Ipv6 n m) = intercalate ":" [quad1, quad2, quad3, quad4, quad5, quad6, quad7, quad8]
  where
    quad1 = hexShow (shiftR n 48 .&. 0xffff)
    quad2 = hexShow (shiftR n 32 .&. 0xffff)
    quad3 = hexShow (shiftR n 16 .&. 0xffff)
    quad4 = hexShow (shiftR n 0 .&. 0xffff)
    quad5 = hexShow (shiftR m 48 .&. 0xffff)
    quad6 = hexShow (shiftR m 32 .&. 0xffff)
    quad7 = hexShow (shiftR m 16 .&. 0xffff)
    quad8 = hexShow (shiftR m 0 .&. 0xffff)

showRule :: PfRule -> Text
showRule (PfRule {..}) = actionString <> directionString <> logString <> quickString <> onString <> afString <> protocolsString <> hostsString <> filterOptsString
  where
    actionString = showAction action <> " "
    directionString = case direction of
      Nothing -> ""
      Just In -> "in "
      Just Out -> "out "
    logString = case logOpts of
      Nothing -> ""
      Just logOpts' -> showLogOpts logOpts' <> " "
    quickString = if quick then "quick " else ""
    onString = case on of
      Nothing -> ""
      Just (Left interfaces) -> "on {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showInterfaceSpec interfaces)) <> "} "
      Just (Right rdomain) -> "on rdomain " <> textShow rdomain <> " "
    afString = case addressFamily of
      Nothing -> ""
      Just Inet -> "inet "
      Just Inet6 -> "inet6 "
    protocolsString = "{" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showProtocol protocols)) <> "} "
    hostsString = case hosts of
      Nothing -> ""
      Just hosts' -> showHosts hosts' <> " "
    filterOptsString = case filterOpts of
      Nothing -> ""
      Just filterOpts' -> showFilterOpts filterOpts'

showFilterOpts :: FilterOpts -> Text
showFilterOpts (FilterOpts {..}) =
  allowString <> divertPortString <> divertReplyString <> divertToString <>
  tcpFlagsString <> groupString <> icmpMatchString <> icmp6MatchString <>
  labelString <> rateLimitString <> onceString <> probabilityString <>
  matchPriorityString <> receivedOnString <> rtableString <>
  setDelayString <> setPriorityString <> setQueueString <> setTagString <>
  matchTagString <> setTOSString <> matchTOSString <> userString <>
  translateToAFString <> translateBiNatString <> translateNatString <>
  translateRdrString <> scrubString <> fragmentString <> stateString <>
  setRouteString
  where
    allowString = if allowOpts then "allow-opts " else ""
    divertPortString = "divert-packet port {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap (showConstraint showPort) divertPort)) <> "} "
    divertReplyString = if divertReply then "divert-reply " else ""
    divertToString = "divert-to " <> showHost (fst divertTo) <> " port {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap (showConstraint showPort) (snd divertTo))) <> "} "
    tcpFlagsString = showFlagMatch tcpFlags
    groupString = "group {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap (showConstraint showGroup) group)) <> "} "
    icmpMatchString = "icmp-type {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showIcmpMatch icmpMatch)) <> "} "
    icmp6MatchString = "icmp6-type {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showIcmp6Match icmp6Match)) <> "} "
    labelString = case label of
      Nothing -> ""
      Just s -> "label " <> pack s <> " "
    rateLimitString = case rateLimit of
      Nothing -> ""
      Just (pkts, secs) -> "max-pkt-rate " <> textShow pkts <> "/" <> textShow secs <> " "
    onceString = if once then "once " else ""
    probabilityString = if probability == 100 then "" else "probability " <> textShow probability <> "% "
    matchPriorityString = case matchPriority of
      Nothing -> ""
      Just n -> "prio " <> textShow n <> " "
    receivedOnString = case receivedOn of
      Nothing -> ""
      Just (neg, iface) -> if neg then "!" else "" <> "received-on " <> showInterface iface <> " "
    rtableString = case rtable of
      Nothing -> ""
      Just n -> "rtable " <> textShow n <> " "
    setDelayString = if setDelay == 0 then "" else "set delay " <> textShow setDelay <> " "
    setPriorityString = case setPriority of
      Nothing -> ""
      Just (n, Nothing) -> "set prio " <> textShow n <> " "
      Just (n, Just m) -> "set prio (" <> textShow n <> ", " <> textShow m <> ") "
    setQueueString = case setQueue of
      Nothing -> ""
      Just (s, Nothing) -> "set queue " <> pack s <> " "
      Just (s, Just t) -> "set queue (" <> pack s <> ", " <> pack t <> ") "
    setTagString = case setTag of
      Nothing -> ""
      Just s -> "tag " <> pack s <> " "
    matchTagString = case matchTag of
      Nothing -> ""
      Just (neg, s) -> (if neg then "!" else "") <> "tagged " <> pack s <> " "
    setTOSString = case setTOS of
      Nothing -> ""
      Just tos -> "set tos " <> showTOS tos <> " "
    matchTOSString = case matchTOS of
      Nothing -> ""
      Just tos -> "tos " <> showTOS tos <> " "
    userString = "user {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap (showConstraint showUser) user)) <> "}"
    translateToAFString = case translateToAF of
      Nothing -> ""
      Just (fam, from, to) -> "af-to " <> showAddressFamily fam <> " from {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showNatHostAddress from)) <> "} to {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showNatHostAddress to)) <> "} "
    translateBiNatString = case translateBiNat of
      (from, port, pool) -> "binat-to {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showNatHostAddress from)) <> "} " <> portSpecString <> poolTypeString
        where
          portSpecString = case port of
            Nothing -> ""
            Just port' -> showPortSpec port' <> " "
          poolTypeString = case pool of
            Nothing -> ""
            Just pool' -> showPoolType pool' <> " "
    translateNatString = case translateNat of
      (from, port, pool, static) -> "nat-to {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showNatHostAddress from)) <> "} " <> portSpecString <> poolTypeString <> staticString
        where
          portSpecString = case port of
            Nothing -> ""
            Just port' -> showPortSpec port' <> " "
          poolTypeString = case pool of
            Nothing -> ""
            Just pool' -> showPoolType pool' <> " "
          staticString = if static then "static-port " else ""
    translateRdrString = case translateRdr of
      (from, port, pool) -> "rdr-to {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showNatHostAddress from)) <> "} " <> portSpecString <> poolTypeString
        where
          portSpecString = case port of
            Nothing -> ""
            Just port' -> showPortSpec port' <> " "
          poolTypeString = case pool of
            Nothing -> ""
            Just pool' -> showPoolType pool' <> " "
    scrubString = "scrub (" <> showScrubOpts scrub <> ") "
    fragmentString = if fragment then "fragment " else "fragment"
    stateString = case state of
      NoState -> "no state "
      KeepState opts -> "keep state " <> case opts of
        Nothing -> ""
        Just opts' -> showStateOpts opts' <> " "
      ModulateState opts -> "modulate state " <> case opts of
        Nothing -> ""
        Just opts' -> showStateOpts opts' <> " "
      SynproxyState opts -> "synproxy state " <> case opts of
        Nothing -> ""
        Just opts' -> showStateOpts opts' <> " "
    setRouteString = case setRoute of
      Nothing -> ""
      Just route -> showRoute route

showPortSpec :: PortSpec -> Text
showPortSpec (PortEqualTo port) = "port " <> showPort port
showPortSpec (PortGreaterThan port) = "port " <> showPort port <> ":*"
showPortSpec (PortRangeInclusive from to) = "port " <> showPort from <> ":" <> showPort to

showNatHostAddress :: (Address, Maybe MaskBits) -> Text
showNatHostAddress (addr, mask) = showHostAddress addr mask Nothing

showAddressFamily :: IsString p => AddressFamily -> p
showAddressFamily Inet = "inet"
showAddressFamily Inet6 = "inet6"

showIcmpMatch :: (Monoid p, IsString p) => IcmpMatch -> p
showIcmpMatch Echorep = "echorep"
showIcmpMatch (Unreach code) = "unreach" <> case code of
  Nothing -> ""
  Just code' -> " code " <> showUnreachCode code'
showIcmpMatch Squench = "squench"
showIcmpMatch (Redir code) = "redir" <> case code of
  Nothing -> ""
  Just code' -> " code " <> showRedirCode code'
showIcmpMatch Althost = "althost"
showIcmpMatch Echoreq = "echoreq"
showIcmpMatch (Routeradv code) = "routeradv" <> case code of
  Nothing -> ""
  Just code' -> " code " <> showRouteradvCode code'
showIcmpMatch Routersol = "routersol"
showIcmpMatch (Timex code) = "timex" <> case code of
  Nothing -> ""
  Just code' -> " code " <> showTimexCode code'
showIcmpMatch (Paramprob code) = "paramprob" <> case code of
  Nothing -> ""
  Just code' -> " code " <> showParamprobCode code'
showIcmpMatch Timereq = "timereq"
showIcmpMatch Timerep = "timerep"
showIcmpMatch Inforeq = "inforeq"
showIcmpMatch Inforep = "inforep"
showIcmpMatch Maskreq = "maskreq"
showIcmpMatch Maskrep = "maskrep"
showIcmpMatch Trace = "trace"
showIcmpMatch Dataconv = "dataconv"
showIcmpMatch Mobredir = "mobredir"
showIcmpMatch Ipv6Where = "ipv6-where"
showIcmpMatch Ipv6Here = "ipv6-here"
showIcmpMatch Mobregreq = "mobregreq"
showIcmpMatch Mobregrep = "mobregrep"
showIcmpMatch Skip = "skip"
showIcmpMatch (Photuris code) = "photuris" <> case code of
  Nothing -> ""
  Just code' -> " code " <> showPhoturisCode code'

showPhoturisCode :: IsString p => PhoturisCode -> p
showPhoturisCode UnknownInd = "unknown-ind"
showPhoturisCode AuthFail = "auth-fail"
showPhoturisCode DecryptFail = "decrypt-fail"

showParamprobCode :: IsString p => ParamprobCode -> p
showParamprobCode Badhead = "badhead"
showParamprobCode Optmiss = "optmiss"
showParamprobCode Badlen = "badlen"

showTimexCode :: IsString p => TimexCode -> p
showTimexCode Transit = "transit"
showTimexCode Reassemb = "reassemb"

showRouteradvCode :: IsString p => RouteradvCode -> p
showRouteradvCode NormalAdv = "normal-adv"
showRouteradvCode CommonAdv = "common-adv"

showRedirCode :: IsString p => RedirCode -> p
showRedirCode RedirNet = "redir-net"
showRedirCode RedirHost = "redir-host"
showRedirCode RedirTosNet = "redir-tos-net"
showRedirCode RedirTosHost = "redir-tos-host"

showIcmp6Match :: (IsString p, Monoid p) => Icmp6Match -> p
showIcmp6Match (Unreach6 code) = "unreach" <> case code of
  Nothing -> ""
  Just code' -> " code " <> showUnreach6Code code'
showIcmp6Match Toobig6 = "toobig"
showIcmp6Match (Timex6 code) = "timex" <> case code of
  Nothing -> ""
  Just code' -> " code " <> showTimex6Code code'
showIcmp6Match (Paramprob6 code) = "paramprob" <> case code of
  Nothing -> ""
  Just code' -> " code " <> showParamprob6Code code'
showIcmp6Match Echoreq6 = "echoreq"
showIcmp6Match Echorep6 = "echorep"
showIcmp6Match Groupqry6 = "groupqry"
showIcmp6Match Listqry6 = "listqry"
showIcmp6Match Grouprep6 = "grouprep"
showIcmp6Match Listenrep6 = "listenrep"
showIcmp6Match Groupterm6 = "groupterm"
showIcmp6Match Listendone6 = "listendone"
showIcmp6Match Routersol6 = "routersol"
showIcmp6Match Routeradv6 = "routeradv"
showIcmp6Match Neighbrsol6 = "neighbrsol"
showIcmp6Match Neighbradv6 = "neighbradv"
showIcmp6Match (Redir6 code) = "redir" <> case code of
  Nothing -> ""
  Just code' -> " code " <> showRedir6Code code'
showIcmp6Match Routrrenum6 = "routrrenum"
showIcmp6Match Fqdnreq6 = "fqdnreq"
showIcmp6Match Niqry6 = "niqry"
showIcmp6Match Wrureq6 = "wrureq"
showIcmp6Match Fqdnrep6 = "fqdnrep"
showIcmp6Match Nirep6 = "nirep"
showIcmp6Match Wrurep6 = "wrurep"
showIcmp6Match Mtraceresp6 = "mtraceresp"
showIcmp6Match Mtrace6 = "mtrace"

showRedir6Code :: IsString p => Redir6Code -> p
showRedir6Code Redironlink6 = "redironlink"
showRedir6Code Redirrouter6 = "redirrouter"

showParamprob6Code :: IsString p => Paramprob6Code -> p
showParamprob6Code Badhead6 = "badhead"
showParamprob6Code Nxthdr6 = "nxthdr"

showTimex6Code :: IsString p => Timex6Code -> p
showTimex6Code Transit6 = "transit"
showTimex6Code Reassemb6 = "reassemb"

showUser :: User -> Text
showUser (UserID w) = textShow w
showUser (UserName s) = pack s

showGroup :: Group -> Text
showGroup (GroupID w) = textShow w
showGroup (GroupName s) = pack s

showFlagMatch :: FlagMatch -> Text
showFlagMatch (FlagMatch m) = "flags " <> onFlagsString <> "/" <> allFlagsString
  where
    (offFlags, onFlags) = Data.Map.partition id m
    offFlagsString = pack (map flagTypeChar (keys offFlags))
    onFlagsString = pack (map flagTypeChar (keys onFlags))
    allFlagsString = offFlagsString <> onFlagsString

flagTypeChar :: FlagType -> Char
flagTypeChar Fin = 'F'
flagTypeChar Syn = 'S'
flagTypeChar Rst = 'R'
flagTypeChar Push = 'P'
flagTypeChar Ack = 'A'
flagTypeChar Urg = 'U'
flagTypeChar Ece = 'E'
flagTypeChar Cwr = 'W'

showRoute :: Route -> Text
showRoute (Route {..}) = methodString <> destinationString <> typeString
  where
    methodString = case method of
      RouteTo -> "route-to "
      ReplyTo -> "reply-to "
      DupTo -> "dup-to "
    destinationString = "{" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showRouteHost destination)) <> "} "
    typeString = case poolType of
      Nothing -> ""
      Just poolType' -> showPoolType poolType'

showPoolType :: PoolType -> Text
showPoolType Bitmask = "bitmask"
showPoolType (LeastStates (StickyAddress s)) = "least-states" <> if s then " sticky-address" else ""
showPoolType (Random (StickyAddress s)) = "random" <> if s then " sticky-address" else ""
showPoolType (RoundRobin (StickyAddress s)) = "round-robin" <> if s then " sticky-address" else ""
showPoolType (SourceHash key) = "source-hash" <> case key of
  Nothing -> ""
  Just s -> " " <> pack s

showRouteHost :: RouteHost -> Text
showRouteHost (RHHost host interface) = hostString <> interfaceString
  where
    hostString = showHost host
    interfaceString = case interface of
      Nothing -> ""
      Just (InterfaceName s) -> "@" <> pack s
showRouteHost (RHInterface (InterfaceName s) address) = "(" <> interfaceString <> addressString <> ")"
  where
    interfaceString = pack s
    addressString = case address of
      Nothing -> ""
      Just (addr, mask) -> " " <> showHostAddress addr mask Nothing

showScrubOpts :: ScrubOpts -> Text
showScrubOpts (ScrubOpts {..}) = intercalate ", " . catMaybes $ [nodfString, minTTLString, maxMSSString, reassembleString, randomIDString]
  where
    nodfString = if nodf then Just "no-df" else Nothing
    minTTLString = case minTTL of
      Nothing -> Nothing
      Just n -> Just ("min-ttl " <> textShow n)
    maxMSSString = case maxMSS of
      Nothing -> Nothing
      Just n -> Just ("max-mss " <> textShow n)
    reassembleString = if reassembleTCP then Just "reassemble tcp" else Nothing
    randomIDString = if randomID then Just "random-id" else Nothing

showTOS :: TOS -> Text
showTOS TOSCritical = "critical"
showTOS InetControl = "inetcontrol"
showTOS LowDelay = "lowdelay"
showTOS NetControl = "netcontrol"
showTOS Throughput = "throughput"
showTOS Reliability = "reliability"
showTOS (DiffServCodePoint s) = pack s
showTOS (DiffServNumber n) = "0x" <> hexShow n

showHosts :: Hosts -> Text
showHosts AllHosts = "all"
showHosts (FromTo hostsFrom portsFrom osFrom hostsTo portsTo) = "from " <> hostsFromString <> portsFromString <> osFromString <> hostsToString <> portsToString
  where
    hostsFromString = "from " <> showHostsFromTo hostsFrom <> " "
    portsFromString = "port {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap (showConstraint showPort) portsFrom)) <> "} "
    osFromString = "os {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showOS osFrom)) <> "} "
    hostsToString = "to " <> showHostsFromTo hostsTo <> " "
    portsToString = "port {" <> Data.Text.intercalate ", " (Data.Array.elems (fmap (showConstraint showPort) portsTo)) <> "}"

showOS :: OSName -> Text
showOS (OSName s) = pack s

showConstraint :: (IsString m, Monoid m) => (t -> m) -> Constraint t -> m
showConstraint show' (EqualTo a) = "= " <> show' a
showConstraint show' (NotEqualTo a) = "!= " <> show' a
showConstraint show' (LessThan a) = "< " <> show' a
showConstraint show' (LessOrEqual a) = "<= " <> show' a
showConstraint show' (GreaterThan a) = "> " <> show' a
showConstraint show' (GreaterOrEqual a) = ">= " <> show' a
showConstraint show' (RangeInclusive a b) = show' a <> ":" <> show' b
showConstraint show' (RangeExclusive a b) = show' a <> " >< " <> show' b
showConstraint show' (NotRangeInclusive a b) = show' a <> " <> " <> show' b

showPort :: PortNumber -> Text
showPort (PortNumber p) = textShow p

showHostsFromTo :: HostsFromTo -> Text
showHostsFromTo HostAny = "any"
showHostsFromTo HostNoRoute = "no-route"
showHostsFromTo HostUrpfFailed = "urpf-failed"
showHostsFromTo HostSelf = "self"
showHostsFromTo (Hosts hosts) = "{" <> Data.Text.intercalate ", " (Data.Array.elems (fmap showHost hosts)) <> "} "
showHostsFromTo (HostRoute s) = "route " <> pack s

showHost :: Host -> Text
showHost (MatchAddress addr mask weight) = showHostAddress addr mask weight
showHost (NotMatchAddress addr mask weight) = "!" <> showHostAddress addr mask weight
showHost (MatchTable (Table t)) = "<" <> pack t <> ">"
showHost (NotMatchTable (Table t)) = "!<" <> pack t <> ">"

showHostAddress :: Address -> Maybe MaskBits -> Maybe Weight -> Text

showHostAddress addr mask weight = addressString <> maskString <> weightString
  where
    addressString = showAddress addr
    maskString = case mask of
      Nothing -> ""
      Just (MaskBits bits) -> "/" <> textShow bits
    weightString = case weight of
      Nothing -> ""
      Just (Weight w) -> " weight " <> textShow w

showAddress :: Address -> Text
showAddress (InterfaceAddress (InterfaceName s)) = pack s
showAddress (GroupAddress (InterfaceGroup s)) = pack s
showAddress (HostnameAddress (Hostname s)) = pack s
showAddress (Ipv4Address ip) = showIpv4 ip
showAddress (Ipv6Address ip) = showIpv6 ip

showProtocol :: Protocol -> Text
showProtocol (Protocol s) = pack s

showLogOpts :: LogOpts -> Text
showLogOpts (LogOpts {..}) = "log " <> allString <> matchesString <> userString <> interfaceString
  where
    allString = if logAll then "all " else ""
    matchesString = if logMatches then "matches " else ""
    userString = if logUser then "user " else ""
    interfaceString = "to " <> pack interface
    (InterfaceName interface) = logInterface

showAction :: Action -> Text
showAction Pass = "pass"
showAction Match = "match"
showAction BlockDefault = "block"
showAction BlockDrop = "block drop"
showAction BlockReturn = "block return"
showAction ReturnRst = "block return-rst"
showAction (ReturnRstTTL ttl) = "block return-rst (ttl " <> textShow ttl <> ")"
showAction (ReturnIcmp4 code) = "return-icmp (" <> showUnreachCode code <> ")"
showAction (ReturnIcmpBoth code code6) = "return-icmp (" <> showUnreachCode code <> ", " <> showUnreach6Code code6 <> ")"
showAction (ReturnIcmp6 code6) = "return-icmp6 (" <> showUnreach6Code code6 <> ")"

showUnreachCode :: IsString p => UnreachCode -> p
showUnreachCode NetUnr = "net-unr"
showUnreachCode HostUnr = "host-unr"
showUnreachCode ProtoUnr = "proto-unr"
showUnreachCode PortUnr = "port-unr"
showUnreachCode Needfrag = "needfrag"
showUnreachCode Srcfail = "srcfail"
showUnreachCode NetUnk = "net-unk"
showUnreachCode HostUnk = "host-unk"
showUnreachCode Isolate = "isolate"
showUnreachCode NetProhib = "net-prohib"
showUnreachCode HostProhib = "host-prohib"
showUnreachCode NetTos = "net-tos"
showUnreachCode HostTos = "host-tos"
showUnreachCode FilterProhib = "filter-prohib"
showUnreachCode HostPreced = "host-preced"
showUnreachCode CutoffPreced = "cutoff-preced"

showUnreach6Code :: IsString p => Unreach6Code -> p
showUnreach6Code NorouteUnr6 = "noroute-unr"
showUnreach6Code AdminUnr6 = "admin-unr"
showUnreach6Code BeyondUnr6 = "beyond-unr"
showUnreach6Code AddrUnr6 = "addr-unr"
showUnreach6Code PortUnr6 = "port-unr"

hexShow :: (Show a, Integral a) => a -> Text
hexShow n = pack (showHex n "")

textShow :: Show a => a -> Text
textShow a = pack (show a)

stringEscape :: Text -> Text
stringEscape t = Data.Text.concatMap (
  \c -> case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    c' -> Data.Text.singleton c') t

