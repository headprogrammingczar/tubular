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

input.preamble
  width: 7em
input.sfd
  width: 1em
input.macdest
  width: 6em
input.macsrc
  width: 6em
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
input.ethernetcrc
  width: 4em
input.gap
  width: 12em
|]) undefined

formtestdoc :: Html
formtestdoc = [shamlet|
<form>
  <div .container .border>
    <h3>Ethernet Frame
    <p>width in bytes
    <div .form-row>
      ^{formInput "Ethernet preamble" "preamble" "preamble"}
      ^{formInput "Start of frame delimiter" "sfd" "sfd"}
    <label for="macdest" .macdest>MAC Addresses
    <div .input-group>
      <div .input-group-between>
        <span .input-group-text>To
      <input #macdest name="macdest" .form-control .macdest>
      <div .input-group-between>
        <span .input-group-text>From
      <input #macsrc name="macsrc" .form-control .macsrc>
    <div .form-row>
      <!-- TODO - this is optional, figure out how to represent it -->
      <!-- TODO - also handle double-tagging -->
      ^{formInput "VLAN tag" "qtag" "qtag"}
      ^{formInput "Double VLAN tag" "doubletag" "doubletag"}
      <!-- TODO - swap out elements based on this value -->
      ^{formInput "EtherType" "ethertype" "ethertype"}
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
      ^{formInput "Frame check sequence" "ethernetcrc" "ethernetcrc"}
      ^{formInput "Interpacket gap" "gap" "gap"}
|]

-- label and input element linked by name, with the same class
formInput :: String -> String -> String -> Html
formInput label name className = [shamlet|
  <div .col>
    <label for="#{name}" .#{className}>#{label}
    <input ##{name} name="#{name}" .form-control .#{className}>
|]

