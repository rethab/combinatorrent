{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Channels
    ( NewPeer(..)
    , PeerChokeMsg(..)
    , MsgTy(..)
    , PeerChannel
    , MgrMessage(..)
    , MgrChannel
    , BandwidthChannel
    , TrackerMsg(..)
    , TrackerChannel
    , peerAddr
    , peerId
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq

import Network.Socket

import Protocol.Wire
import Torrent

-- | an unconnected peer.
--   a peer may come either as bin peer or dict peer. the dict peer also
--   has a peer id associated as comes as a bencoded dictionary whereas
--   the bin peer is only a number sequence of ip and port
data NewPeer = NewBinPeer SockAddr -- ^ a fresh bin peer before the crypto handshake
             | NewDictPeer PeerPeerId SockAddr -- ^ a fresh dict peer

peerAddr :: NewPeer -> SockAddr
peerAddr (NewBinPeer a) = a
peerAddr (NewDictPeer _ a) = a

peerId :: NewPeer -> PeerId
peerId (NewBinPeer _) = "UNKNOWN"
peerId (NewDictPeer (PPID p) _) = p

data MsgTy = FromPeer (Message, Int)
           | FromSenderQ Int -- Always UpRate events
           | FromChokeMgr PeerChokeMsg
           | TimerTick

data PeerChokeMsg = ChokePeer
                  | UnchokePeer
                  | PieceCompleted PieceNum
                  | CancelBlock PieceNum Block

type PeerChannel = TChan MsgTy

instance NFData PeerChannel where
    rnf pc = pc `seq` ()

---- TRACKER

-- | Messages to the tracker process
data TrackerMsg = Stop -- ^ Ask the Tracker to stop
                | TrackerTick Integer -- ^ Ticking in the tracker, used to contact again
                | Start               -- ^ Ask the tracker to Start
                | Complete            -- ^ Ask the tracker to Complete the torrent
type TrackerChannel = TChan TrackerMsg

data MgrMessage = Connect InfoHash ThreadId PeerChannel
                | Disconnect ThreadId

type MgrChannel = TChan MgrMessage

-- | A Channel type we use for transferring the amount of data we transferred
type BandwidthChannel = TChan Integer
