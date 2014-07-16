module Process.Listen
    ( start
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (bracketOnError)
import Control.Monad.Reader

import Network hiding (accept, sClose)
import Network.Socket
import Network.BSD

import Process
import Process.PeerMgr hiding (start)
import Supervisor
import Torrent

data CF = CF { peerMgrCh :: PeerMgrChannel }

instance Logging CF where
    logName _ = "Process.Listen"

start :: Port -> PeerMgrChannel -> SupervisorChannel -> IO ThreadId
start port peerMgrC supC = do
    spawnP (CF peerMgrC) () ({-# SCC "Listen" #-} catchP (openListen port >>= eventLoop)
                        (defaultStopHandler supC)) -- TODO: Close socket resource!

openListen :: Port -> Process CF () Socket
openListen (P port) = liftIO $ do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet (toEnum $ fromIntegral port) iNADDR_ANY)
            listen sock maxListenQueue
            return sock
        )

eventLoop :: Socket -> Process CF () ()
eventLoop sockFd = do
    c <- asks peerMgrCh
    liftIO $ do
        conn <- accept sockFd
        atomically $ writeTChan c (NewIncoming conn)
    eventLoop sockFd

