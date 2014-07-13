module Process.Peer.Sender
  ( start )
where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader

import qualified Data.ByteString.Lazy as L

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)

import Process
import Supervisor
import Crypto

data CF = CF { chan :: TMVar L.ByteString
             , sock :: ConnectedPeer }

instance Logging CF where
    logName _ = "Process.Peer.Sender"

-- | The raw sender process, it does nothing but send out what it syncs on.
start :: ConnectedPeer -> TMVar L.ByteString -> SupervisorChannel -> IO ThreadId
start connP@(ConnectedPeer s _) ch supC = spawnP (CF ch connP) () ({-# SCC "Sender" #-}
                                          (cleanupP pgm
                                            (defaultStopHandler supC)
                                            (liftIO $ sClose s)))
pgm :: Process CF () ()
pgm = do
   ch <- asks chan
   connP@(ConnectedPeer s _) <- asks sock
   _ <- liftIO $ do
      r <- atomically $ takeTMVar ch
      sendAll s (encryptL connP r)
   pgm

