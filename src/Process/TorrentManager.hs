-- | The Manager Process - Manages the torrents and controls them
module Process.TorrentManager (
    -- * Types
      TorrentManagerMsg(..)
    -- * Channels
    , TorrentMgrChan
    -- * Interface
    , start
    )
where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.ByteString as B
import Prelude hiding (log)

import Protocol.BCode as BCode
import Process
import qualified Process.Status as Status
import qualified Process.PeerMgr as PeerMgr
import qualified Process.FS as FSP
import qualified Process.PieceMgr as PieceMgr (start, createPieceDb)
import qualified Process.ChokeMgr as ChokeMgr (ChokeMgrChannel)
import qualified Process.Tracker as Tracker
import Channels
import FS
import Supervisor
import Torrent

data TorrentManagerMsg = AddedTorrent FilePath
                       | RemovedTorrent FilePath
  deriving (Eq, Show)

type TorrentMgrChan = TChan [TorrentManagerMsg]

data CF = CF { tCh :: TorrentMgrChan
             , tStatusCh    :: Status.StatusChannel
             , tStatusTV    :: TVar [Status.PStat]
             , tPeerId      :: MyPeerId
             , tPeerMgrCh   :: PeerMgr.PeerMgrChannel
             , tChokeCh     :: ChokeMgr.ChokeMgrChannel
             }

instance Logging CF where
  logName _ = "Process.TorrentManager"

data ST = ST { workQueue :: [TorrentManagerMsg] }
start :: TorrentMgrChan -- ^ Channel to watch for changes to torrents
      -> Status.StatusChannel
      -> TVar [Status.PStat]
      -> ChokeMgr.ChokeMgrChannel
      -> MyPeerId
      -> PeerMgr.PeerMgrChannel
      -> SupervisorChannel
      -> IO ThreadId
start chan statusC stv chokeC pid peerC supC =
    spawnP (CF chan statusC stv pid peerC chokeC) (ST [])
                ({-# SCC "TorrentManager" #-} catchP pgm (defaultStopHandler supC))
  where pgm = startStop >> dirMsg >> pgm
        dirMsg = do
            c <- asks tCh
            ls <- liftIO . atomically $ readTChan c
            modify (\s -> s { workQueue = ls ++ workQueue s })
        startStop = do
            q <- gets workQueue
            case q of
                [] -> return ()
                (AddedTorrent fp : rest) -> do
                    debugP $ "Adding torrent file: " ++ fp
                    _ <- startTorrent fp
                    modify (\s -> s { workQueue = rest })
                    startStop
                (RemovedTorrent _ : _) -> do
                    errorP "Removal of torrents not yet supported :P"
                    stopP

readTorrent :: FilePath -> Process CF ST BCode
readTorrent fp = do
    torrent <- liftIO $ B.readFile fp
    let bcoded = BCode.decode torrent
    case bcoded of
      Left err -> do liftIO $ print err
                     stopP
      Right bc -> return bc

startTorrent :: FilePath -> Process CF ST (Maybe ThreadId)
startTorrent fp = do
    bc <- readTorrent fp
    ti <- liftIO $ mkTorrentInfo bc
    sts <- do v <- liftIO newEmptyTMVarIO
              statusC <- asks tStatusCh
              liftIO . atomically $ writeTChan statusC (Status.RequestAllTorrents v)
              liftIO . atomically $ takeTMVar v
    case lookup (infoHash ti) sts of
      Nothing -> Just `fmap` startTorrent' fp bc ti
      Just _x  -> return Nothing

startTorrent' :: [Char] -> BCode -> TorrentInfo -> Process CF ST ThreadId
startTorrent' fp bc ti = do
    fspC     <- liftIO newTChanIO
    trackerC <- liftIO newTChanIO
    supC     <- liftIO newTChanIO
    pieceMgrC  <- liftIO newTChanIO
    chokeC  <- asks tChokeCh
    statusC <- asks tStatusCh
    stv <- asks tStatusTV
    pid     <- asks tPeerId
    pmC     <- asks tPeerMgrCh
    (handles, haveMap, pieceMap) <- liftIO $ openAndCheckFile bc
    let left = bytesLeft haveMap pieceMap
    pieceDb <- PieceMgr.createPieceDb haveMap pieceMap
    (tid, _) <- liftIO $ allForOne ("TorrentSup - " ++ fp)
                     [ Worker $ FSP.start handles pieceMap fspC
                     , Worker $ PieceMgr.start pieceMgrC fspC chokeC statusC pieceDb (infoHash ti)
                     , Worker $ Tracker.start (infoHash ti) ti pid defaultPort statusC trackerC pmC
                     ] supC
    liftIO . atomically $ writeTChan statusC $ Status.InsertTorrent (infoHash ti) left trackerC
    c <- asks tPeerMgrCh
    liftIO . atomically $ writeTChan c $ PeerMgr.NewTorrent (infoHash ti)
                            (PeerMgr.TorrentLocal pieceMgrC fspC stv pieceMap)
    liftIO . atomically $ writeTChan trackerC Start
    return tid
