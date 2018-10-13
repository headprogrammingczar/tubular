{-# LANGUAGE DuplicateRecordFields #-}
module Pf where

import Data.TotalMap
import Data.Map

-- data structure that fully specifies https://man.openbsd.org/pf.conf
{-
Syntax for pf.conf in BNF:

line           = ( option | pf-rule |
                 antispoof-rule | queue-rule | anchor-rule |
                 anchor-close | load-anchor | table-rule | include )

option         = "set" ( [ "timeout" TimeoutDefaults ] |
                 [ "ruleset-optimization" [ RulesetOptimizationLevel ] ] |
                 [ "optimization" [ OptimizationLevel ] ]
                 [ "limit" Limits ] |
                 [ "loginterface" (Maybe InterfaceName) ] |
                 [ "block-policy" BlockPolicy ] |
                 [ "state-policy" StatePolicy ]
                 [ "state-defaults" state-opts ]
                 [ "fingerprints" filename ] |
                 [ "skip on" ifspec ] |
                 [ "debug" DebugLevel ] |
                 [ "reassemble" Bool [ "no-df" ] ] )

pf-rule        = action [ Direction ]
                 [ "log" [ "(" logopts ")"] ] [ "quick" ]
                 [ "on" ( ifspec | "rdomain" number ) ] [ AddressFamily ]
                 [ protospec ] [ hosts ] [ filteropts ]

logopts        = logopt [ [ "," ] logopts ]
logopt         = "all" | "matches" | "user" | "to" InterfaceName

filteropts     = filteropt [ [ "," ] filteropts ]
filteropt      = user | group | FlagMatch | icmp-type | icmp6-type |
                 "tos" tos |
                 ( "no" | "keep" | "modulate" | "synproxy" ) "state"
                 [ "(" state-opts ")" ] | "scrub" "(" scrubopts ")" |
                 "fragment" | "allow-opts" | "once" |
                 "divert-packet" "port" port | "divert-reply" |
                 "divert-to" host "port" port |
                 "label" string | "tag" string | [ "!" ] "tagged" string |
                 "max-pkt-rate" number "/" seconds |
                 "set delay" number |
                 "set prio" ( number | "(" number [ [ "," ] number ] ")" ) |
                 "set queue" ( string | "(" string [ [ "," ] string ] ")" ) |
                 "rtable" number | "probability" number"%" | "prio" number |
                 "af-to" AddressFamily "from" ( redirhost | "{" redirhost-list "}" )
                 [ "to" ( redirhost | "{" redirhost-list "}" ) ] |
                 "binat-to" ( redirhost | "{" redirhost-list "}" )
                 [ portspec ] [ pooltype ] |
                 "rdr-to" ( redirhost | "{" redirhost-list "}" )
                 [ portspec ] [ pooltype ] |
                 "nat-to" ( redirhost | "{" redirhost-list "}" )
                 [ portspec ] [ pooltype ] [ "static-port" ] |
                 [ route ] | [ "set tos" tos ] |
                 [ [ "!" ] "received-on" ( InterfaceName | InterfaceGroup ) ]

scrubopts      = scrubopt [ [ "," ] scrubopts ]
scrubopt       = "no-df" | "min-ttl" number | "max-mss" number |
                 "reassemble tcp" | "random-id"

antispoof-rule = "antispoof" [ "log" ] [ "quick" ]
                 "for" ifspec [ AddressFamily ] [ "label" string ]

table-rule     = "table" "<" string ">" [ tableopts ]
tableopts      = tableopt [ tableopts ]
tableopt       = "persist" | "const" | "counters" |
                 "file" string | "{" [ tableaddrs ] "}"
tableaddrs     = tableaddr-spec [ [ "," ] tableaddrs ]
tableaddr-spec = [ "!" ] tableaddr [ "/" mask-bits ]
tableaddr      = Hostname | ifspec | "self" |
                 Ipv4 | Ipv6

queue-rule     = "queue" string [ "on" InterfaceName ] queueopts-list

anchor-rule    = "anchor" [ string ] [ Direction ] [ "on" ifspec ]
                 [ AddressFamily ] [ protospec ] [ hosts ] [ filteropt-list ] [ "{" ]

anchor-close   = "}"

load-anchor    = "load anchor" string "from" filename

queueopts-list = queueopts-list queueopts | queueopts
queueopts      = ([ "bandwidth" Bandwidth ] | [ "min" Bandwidth ] |
                 [ "max" Bandwidth ] | [ "parent" string ] |
                 [ "default" ]) |
                 ([ "flows" number ] | [ "quantum" number ]) |
                 [ "qlimit" number ]

action         = "pass" | "match" | "block" [ return ]
return         = "drop" | "return" |
                 "return-rst" [ "(" "ttl" number ")" ] |
                 "return-icmp" [ "(" icmpcode [ [ "," ] icmp6code ] ")" ] |
                 "return-icmp6" [ "(" icmp6code ")" ]
icmpcode       = ( icmp-code-name | icmp-code-number )
icmp6code      = ( icmp6-code-name | icmp6-code-number )

ifspec         = ( [ "!" ] ( InterfaceName | InterfaceGroup ) ) |
                 "{" interface-list "}"
interface-list = [ "!" ] ( InterfaceName | InterfaceGroup )
                 [ [ "," ] interface-list ]
route          = ( "route-to" | "reply-to" | "dup-to" )
                 ( routehost | "{" routehost-list "}" )
                 [ pooltype ]

protospec      = "proto" ( proto-name | proto-number |
                 "{" proto-list "}" )
proto-list     = ( proto-name | proto-number ) [ [ "," ] proto-list ]

hosts          = "all" |
                 "from" ( "any" | "no-route" | "urpf-failed" | "self" |
                 host | "{" host-list "}" | "route" string ) [ port ]
                 [ os ]
                 "to"   ( "any" | "no-route" | "self" | host |
                 "{" host-list "}" | "route" string ) [ port ]

ipspec         = "any" | host | "{" host-list "}"
host           = [ "!" ] ( Address [ "weight" number ] |
                 Address [ "/" mask-bits ] [ "weight" number ] |
                 "<" string ">" )
redirhost      = Address [ "/" mask-bits ]
routehost      = host | host "@" InterfaceName |
                 "(" InterfaceName [ Address [ "/" mask-bits ] ] ")"
host-list      = host [ [ "," ] host-list ]
redirhost-list = redirhost [ [ "," ] redirhost-list ]
routehost-list = routehost [ [ "," ] routehost-list ]

port           = "port" ( unary-op | binary-op | "{" op-list "}" )
portspec       = "port" ( number | name ) [ ":" ( "*" | number | name ) ]
os             = "os"  ( os-name | "{" os-list "}" )
user           = "user" ( unary-op | binary-op | "{" op-list "}" )
group          = "group" ( unary-op | binary-op | "{" op-list "}" )

unary-op       = [ "=" | "!=" | "<" | "<=" | ">" | ">=" ]
                 ( name | number )
binary-op      = number ( "<>" | "><" | ":" ) number
op-list        = ( unary-op | binary-op ) [ [ "," ] op-list ]

os-name        = operating-system-name
os-list        = os-name [ [ "," ] os-list ]

icmp-type      = "icmp-type" ( Array IcmpMatch )
icmp6-type     = "icmp6-type" ( Array Icmp6Match )

tos            = ( "lowdelay" | "throughput" | "reliability" |
                 [ "0x" ] number )

state-opts     = state-opt [ [ "," ] state-opts ]
state-opt      = ( "max" number | "no-sync" | (Timeout, Word) | "sloppy" |
                 "pflow" | "source-track" [ ( "rule" | "global" ) ] |
                 "max-src-nodes" number | "max-src-states" number |
                 "max-src-conn" number |
                 "max-src-conn-rate" number "/" number |
                 "overload" "<" string ">" [ "flush" [ "global" ] ] |
                 "if-bound" | "floating" )

pooltype       = ( "bitmask" | "least-states" |
                 "random" | "round-robin" |
                 "source-hash" [ ( hex-key | string-key ) ] )
                 [ "sticky-address" ]

include        = "include" filename
-}

{-
data PfConf = PfConf {pfRules :: [PfRule]}

data PfRule = Rule {policy :: Policy}

data Action =
  Block |
  Match |
  Pass

data Parameter =
  In |
  Out |
  Log LogMatch |
  Quick |
  On OnMatch |
  Inet |
  Inet6 |
  Proto ProtoMatch |
  FromTo FromToMatch

data AdditionalParameter =
  All |
  AllowOpts |
  DivertPacket Port |
  DivertReply |
  DivertTo Host Port |
  Flags FlagMatch |
  Group GroupMatch |
  IcmpType Type Code |
  Icmp6Type Type Code |
  Label LabelMatch |
  MaxPacketRate Rate |
  Once |
  Probability Percent |
  Priority PriorityMatch |
  ReceivedOn ReceivedMatch |
  RoutingTable TableMatch |
  SetDelay Milliseconds |
  SetPriority Priority |
  SetQueue Queue |
  SetTos Tos |
  Tag Tag |
  Tagged TagMatch |
  Tos TosMatch |
  User UserMatch

data RoutingParameter =
  Dup |
  Reply |
  Route

data GlobalOptions = GlobalOptions {
  blockPolicy :: BlockPolicy,
  debugLevel :: DebugLevel,
  fingerprintsFile :: FingerprintsFilepath,
  hostid :: HostID,
  resourceLimits :: ResourceLimits,
  logInterface :: LogInterface,
  optimization :: Optimization,
  reassemble :: ReassembleOption,
  rulesetOptimization :: RulesetOptimization,
  skip :: SkipOptions,
  stateDefaults :: StateDefaults,
  statePolicy :: StatePolicy,
  syncCookies :: SyncCookiesMode,
  timeouts :: TimeoutDefaults
}
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

data TimeoutDefaults = TimeoutDefaults (TMap TimeoutType Word)

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

data Paramprob6Code =
  Badhead6 |
  Nxthdr6 |
  Paramprob6Code2

data Redir6Code =
  Redironlink6 |
  Redirrouter6

-- the name of an interface such as "en0"
data InterfaceName = InterfaceName String

-- must be 1-15 characters and cannot end in a number
data InterfaceGroup = InterfaceGroup String

data Hostname = Hostname String

-- packed ipv4 address
data Ipv4 = Ipv4 Word32

-- packed-ish ipv6 address - there is no Word128
data Ipv6 = Ipv6 Word64 Word64

data Address =
  InterfaceAddress InterfaceName |
  GroupAddress InterfaceGroup |
  HostnameAddress Hostname |
  Ipv4Address Ipv4 |
  Ipv6Address Ipv6

