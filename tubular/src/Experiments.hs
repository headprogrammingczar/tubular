{-# LANGUAGE QuasiQuotes #-}
module Experiments where

import Text.Cassius
import Text.Hamlet

-- there's no "simple" css QQ, so apply it to undefined
-- maybe this should be a mixin...
formtestcss :: Css
formtestcss = ([cassius|
.input-group-between
    display: -ms-flexbox
    display: flex
.input-group > .input-group-between:not(:first-child) > .input-group-text
    margin-left: -1px
    border-top-left-radius: 0
    border-bottom-left-radius: 0
.input-group > .input-group-between:not(:last-child) > .input-group-text
    margin-right: -1px
    border-top-right-radius: 0
    border-bottom-right-radius: 0

input.qtag
  width: 4em
input.doubletag
  width: 4em
input.ethertype
  width: 2em
input.ipver
  width: 4em
input.ihl
  width: 4em
input.dscp
  width: 6em
input.ecn
  width: 2em
input.length
  width: 16em
input.identification
  width: 16em
input.flags
  width: 3em
input.fragmentoffset
  width: 13em
input.ttl
  width: 8em
input.protocol
  width: 8em
input.checksum
  width: 16em
input.ipsrc
  width: 32em
input.ipdst
  width: 32em
input.ipv4options
  width: 100%
input.sport
  width: 16em
input.dport
  width: 16em
input.seqnum
  width: 32em
input.acknum
  width: 32em
input.dataoffset
  width: 4em
input.reserved
  width: 3em
input.tcpflags
  width: 9em
input.windowsize
  width: 16em
input.tcpchecksum
  width: 16em
input.urgentptr
  width: 16em
input.tcpoptions
  width: 40em
|]) undefined

formtestdoc :: Html
formtestdoc = [shamlet|
<form>
  <div .container .border>
    <h3>Ethernet Frame
    <p>width in bytes
    <div .form-row>
      <label .preamble>Ethernet Preamble
      <div .input-group>
        <div .input-group-between>
          <span .input-group-text>Preamble
        <div .input-group-between>
          <span .input-group-text>10101010 10101010 10101010 10101010 10101010 10101010 10101010
        <div .input-group-between>
          <span .input-group-text>Start of Frame Delimiter
        <input #sfd name="sfd" .form-control .sfd disabled value="10101011">
    <div .form-row>
      <label for="macdest" .macdest>MAC Addresses
      <div .input-group>
        <div .input-group-between>
          <span .input-group-text>To
        <input #macdest name="macdest" .form-control .macdest>
        <div .input-group-between>
          <span .input-group-text>From
        <input #macsrc name="macsrc" .form-control .macsrc>
    <div .form-row>
      <!-- TODO - swap out elements based on this value -->
      <label .ethertype1>EtherType
      <select #ethertype1 name="ethertype1" .custom-select onchange="on_ethertype1_change()">
        <option value="0x0800">0x0800 Internet Protocol version 4 (IPv4)
        <option value="0x0806">0x0806 Address Resolution Protocol (ARP)
        <option value="0x0842">0x0842 Wake-on-LAN
        <option value="0x22F3">0x22F3 IETF TRILL Protocol
        <option value="0x22EA">0x22EA Stream Reservation Protocol
        <option value="0x6003">0x6003 DECnet Phase IV
        <option value="0x8035">0x8035 Reverse Address Resolution Protocol
        <option value="0x809B">0x809B AppleTalk (Ethertalk)
        <option value="0x80F3">0x80F3 AppleTalk Address Resolution Protocol (AARP)
        <option value="0x8100">0x8100 VLAN-tagged frame (IEEE 802.1Q) and Shortest Path Bridging IEEE 802.1aq with NNI compatibility
        <option value="0x8137">0x8137 IPX
        <option value="0x8204">0x8204 QNX Qnet
        <option value="0x86DD">0x86DD Internet Protocol Version 6 (IPv6)
        <option value="0x8808">0x8808 Ethernet flow control
        <option value="0x8809">0x8809 Ethernet Slow Protocols
        <option value="0x8819">0x8819 CobraNet
        <option value="0x8847">0x8847 MPLS unicast
        <option value="0x8848">0x8848 MPLS multicast
        <option value="0x8863">0x8863 PPPoE Discovery Stage
        <option value="0x8864">0x8864 PPPoE Session Stage
        <option value="0x886D">0x886D Intel Advanced Networking Services
        <option value="0x8870">0x8870 Jumbo Frames (Obsoleted draft-ietf-isis-ext-eth-01)
        <option value="0x887B">0x887B HomePlug 1.0 MME
        <option value="0x888E">0x888E EAP over LAN (IEEE 802.1X)
        <option value="0x8892">0x8892 PROFINET Protocol
        <option value="0x889A">0x889A HyperSCSI (SCSI over Ethernet)
        <option value="0x88A2">0x88A2 ATA over Ethernet
        <option value="0x88A4">0x88A4 EtherCAT Protocol
        <option value="0x88A8">0x88A8 Provider Bridging (IEEE 802.1ad) & Shortest Path Bridging IEEE 802.1aq
        <option value="0x88AB">0x88AB Ethernet Powerlink[citation needed]
        <option value="0x88B8">0x88B8 GOOSE (Generic Object Oriented Substation event)
        <option value="0x88B9">0x88B9 GSE (Generic Substation Events) Management Services
        <option value="0x88BA">0x88BA SV (Sampled Value Transmission)
        <option value="0x88CC">0x88CC Link Layer Discovery Protocol (LLDP)
        <option value="0x88CD">0x88CD SERCOS III
        <option value="0x88DC">0x88DC WSMP, WAVE Short Message Protocol
        <option value="0x88E1">0x88E1 HomePlug AV MME[citation needed]
        <option value="0x88E3">0x88E3 Media Redundancy Protocol (IEC62439-2)
        <option value="0x88E5">0x88E5 MAC security (IEEE 802.1AE)
        <option value="0x88E7">0x88E7 Provider Backbone Bridges (PBB) (IEEE 802.1ah)
        <option value="0x88F7">0x88F7 Precision Time Protocol (PTP) over Ethernet (IEEE 1588)
        <option value="0x88F8">0x88F8 NC-SI
        <option value="0x88FB">0x88FB Parallel Redundancy Protocol (PRP)
        <option value="0x8902">0x8902 IEEE 802.1ag Connectivity Fault Management (CFM) Protocol / ITU-T Recommendation Y.1731 (OAM)
        <option value="0x8906">0x8906 Fibre Channel over Ethernet (FCoE)
        <option value="0x8914">0x8914 FCoE Initialization Protocol
        <option value="0x8915">0x8915 RDMA over Converged Ethernet (RoCE)
        <option value="0x891D">0x891D TTEthernet Protocol Control Frame (TTE)
        <option value="0x892F">0x892F High-availability Seamless Redundancy (HSR)
        <option value="0x9000">0x9000 Ethernet Configuration Testing Protocol
        <option value="0x9100">0x9100 VLAN-tagged (IEEE 802.1Q) frame with double tagging
      <!-- TODO - this is optional, figure out how to represent it -->
      <div #ethtype2-div style="display: none">
        ^{formInput "Nested EtherType" "qtag" "qtag"}
      <!-- TODO - also handle double-tagging -->
      <div #ethtype3-div style="display: none">
        ^{formInput "Double Nested EtherType" "doubletag" "doubletag"}
    <div .container .border>
      <h3>IPv4 Packet
      <p>width in bits
      <div .form-row>
        ^{formInput "IP version" "ipver" "ipver"}
        ^{formInput "Internet header length" "ihl" "ihl"}
        ^{formInput "Differentiated services code point" "dscp" "dscp"}
        ^{formInput "Explicit congestion notification" "ecn" "ecn"}
        ^{formInput "Total length" "length" "length"}
        ^{formInput "Identification" "identification" "identification"}
        ^{formInput "Flags" "flags" "flags"}
        ^{formInput "Fragment offset" "fragmentoffset" "fragmentoffset"}
        ^{formInput "TTL" "ttl" "ttl"}
        ^{formInput "Protocol" "protocol" "protocol"}
        ^{formInput "Header checksum" "checksum" "checksum"}
      <label for="ipsrc" .ipsrc>IP Addresses
      <div .input-group>
        <div .input-group-between>
          <span .input-group-text>From
        <input #ipsrc name="ipsrc" .form-control .ipsrc>
        <div .input-group-between>
          <span .input-group-text>To
        <input #ipdst name="ipdst" .form-control .ipdst>
      <div .form-row>
        ^{formInput "Options" "ipv4options" "ipv4options"}
      <div .container .border>
        <h3>TCP Segment
        <p>width in bits
        <label for="sport" .sport>Port Numbers
        <div .input-group>
          <div .input-group-between>
            <span .input-group-text>From
          <input #sport name="sport" .form-control .sport>
          <div .input-group-between>
            <span .input-group-text>To
          <input #dport name="dport" .form-control .dport>
        <div .form-row>
          ^{formInput "Sequence number" "seqnum" "seqnum"}
          ^{formInput "Acknowledgement number" "acknum" "acknum"}
        <div .form-row>
          ^{formInput "Data offset" "dataoffset" "dataoffset"}
          ^{formInput "Reserved" "reserved" "reserved"}
          ^{formInput "Flags" "tcpflags" "tcpflags"}
          ^{formInput "Window size" "windowsize" "windowsize"}
          ^{formInput "Checksum" "tcpchecksum" "tcpchecksum"}
          ^{formInput "Urgent pointer" "urgentptr" "urgentptr"}
          ^{formInput "Options" "tcpoptions" "tcpoptions"}
          ^{formInput "Payload" "payload" "payload"}
    <div .form-row>
      <label .preamble>Frame Check Sequence
      <input #ethernetcrc name="ethernetcrc" .form-control .gap disabled value="00111011 01001011 00111100 01000111">
    <div .form-row>
      <label .preamble>Interpacket Gap
      <input #gap name="gap" .form-control .gap disabled value="00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000">
|]

-- label and input element linked by name, with the same class
formInput :: String -> String -> String -> Html
formInput label name className = [shamlet|
  <div .col>
    <label for="#{name}" .#{className}>#{label}
    <input ##{name} name="#{name}" .form-control .#{className}>
|]

