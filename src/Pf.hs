{-# LANGUAGE DuplicateRecordFields #-}
module Pf where

import Data.TotalMap
import Data.Map
import Data.Word
import Data.Array

-- UNDOCUMENTED
-- in the grammar, ipspec is completely unused

-- data structure that fully specifies https://man.openbsd.org/pf.conf
{-
Syntax for pf.conf in BNF:

line           = ( Options | pf-rule |
                 antispoof-rule | queue-rule | anchor-rule |
                 anchor-close | load-anchor | TableRule | Include )

pf-rule        = Action [ Direction ]
                 [ "log" [ "(" LogOpts ")"] ] [ "quick" ]
                 [ "on" ( (Array InterfaceSpec) | "rdomain" number ) ] [ AddressFamily ]
                 [ (Array Protocol) ] [ hosts ] [ filteropts ]

filteropts     = filteropt [ [ "," ] filteropts ]
filteropt      = user | group | FlagMatch | (Array IcmpMatch) | (Array Icmp6Match) |
                 "tos" TOS |
                 ( "no" | "keep" | "modulate" | "synproxy" ) "state"
                 [ StateOptions ] | "scrub" "(" scrubopts ")" |
                 "fragment" | "allow-opts" | "once" |
                 "divert-packet" "port" (OrArray (Constraint PortNumber)) | "divert-reply" |
                 "divert-to" Host "port" (OrArray (Constraint PortNumber)) |
                 "label" string | "tag" string | [ "!" ] "tagged" string |
                 "max-pkt-rate" number "/" seconds |
                 "set delay" number |
                 "set prio" ( number | "(" number [ [ "," ] number ] ")" ) |
                 "set queue" ( string | "(" string [ [ "," ] string ] ")" ) |
                 "rtable" number | "probability" number"%" | "prio" number |
                 "af-to" AddressFamily "from" ( "{" redirhost-list "}" )
                 [ "to" ( "{" redirhost-list "}" ) ] |
                 "binat-to" ( "{" redirhost-list "}" )
                 [ PortSpec ] [ PoolType ] |
                 "rdr-to" ( "{" redirhost-list "}" )
                 [ PortSpec ] [ PoolType ] |
                 "nat-to" ( "{" redirhost-list "}" )
                 [ PortSpec ] [ PoolType ] [ "static-port" ] |
                 [ Route ] | [ "set tos" TOS ] |
                 [ [ "!" ] "received-on" ( InterfaceName | InterfaceGroup ) ]

scrubopts      = scrubopt [ [ "," ] scrubopts ]
scrubopt       = "no-df" | "min-ttl" number | "max-mss" number |
                 "reassemble tcp" | "random-id"

antispoof-rule = "antispoof" [ "log" ] [ "quick" ]
                 "for" (Array InterfaceSpec) [ AddressFamily ] [ "label" string ]

queue-rule     = "queue" string [ "on" InterfaceName ] queueopts-list

anchor-rule    = "anchor" [ string ] [ Direction ] [ "on" (Array InterfaceSpec) ]
                 [ AddressFamily ] [ (Array Protocol) ] [ hosts ] [ filteropt-list ] [ "{" ]

anchor-close   = "}"

load-anchor    = "load anchor" string "from" filename

queueopts-list = queueopts-list queueopts | queueopts
queueopts      = ([ "bandwidth" Bandwidth ] | [ "min" Bandwidth ] |
                 [ "max" Bandwidth ] | [ "parent" string ] |
                 [ "default" ]) |
                 ([ "flows" number ] | [ "quantum" number ]) |
                 [ "qlimit" number ]

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
hosts          = "all" |
                 "from" ( "any" | "no-route" | "urpf-failed" | "self" |
                 (Array Host) | "route" string ) [ (OrArray (Constraint PortNumber)) ]
                 [ os ]
                 "to"   ( "any" | "no-route" | "self" |
                 (Array Host) | "route" string ) [ (OrArray (Constraint PortNumber)) ]

host           = Host
redirhost-list = Array (Address, Maybe MaskBits)

-- UNDOCUMENTED
-- constraints are OR'd together
-- for instance, pf implements "port {80, 443}" with one rule for each port
os             = (OrArray OSName)
user           = "user" (OrArray (Constraint User))
group          = "group" (OrArray (Constraint Group))
-}

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
-- type OrArray = Array

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

data Group = GroupID Word32 | GroupName String

data Action =
  Pass |
  Match |
  -- uses block-policy global option
  BlockDefault |
  BlockDrop |
  BlockReturn |
  -- UNDOCUMENTED$
  -- ttl could be made more clear to someone not deeply familiar with IP(v4/v6)$
  -- it sets return_ttl on a struct, which is type u_int8_t$
  -- this probably means it is https://en.wikipedia.org/wiki/Time_to_live$
  -- ttl seems to default to 0? it probably hits scrub min-ttl rules too
  ReturnRst |
  ReturnRstTTL Word8 |
  -- UNDOCUMENTED$
  -- pf.conf(5) does not make this very clear, but the source does$
  -- in /sbin/pfctl/parse.y these options take icmp codes, which depend on a type code
  -- the code is between 0 and 255, and is combined with the type of returnicmpdefault$
  -- returnicmpdefault is (ICMP_UNREACH << 8) | ICMP_UNREACH_PORT$
  -- or in haskell, (Unreach PortUnr)$
  -- returnicmp6default is (ICMP6_DST_UNREACH << 8) | ICMP6_DST_UNREACH_NOPORT$
  -- or in haskell, (Unreach6 PortUnr6)$
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

data StateOptions = StateOptions {
  max :: Word,
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
  const :: Bool,
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

