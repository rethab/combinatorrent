module Process.Peer.Receiver
    ( start )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (assert)

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString as B
import Prelude hiding (log)

import Data.Serialize.Get

import Network.Socket.ByteString

import Channels
import Crypto
import Process
import Supervisor
import Protocol.Wire


data CF = CF { rpMsgCh :: TChan MsgTy }

instance Logging CF where
    logName _ = "Process.Peer.Receiver"

demandInput :: Int -> Process CF ConnectedPeer B.ByteString
demandInput l = {-# SCC "demandInput" #-} do
    connP@(ConnectedPeer s _) <- get
    bs <- liftIO $ decrypt connP `fmap` recv s (fromIntegral l)
    when (B.null bs) stopP
    return bs

start :: ConnectedPeer -> TChan MsgTy -> SupervisorChannel -> IO ThreadId
start s ch supC = do
   spawnP (CF ch) s
        ({-# SCC "Receiver" #-} catchP readSend
               (defaultStopHandler supC))

readSend :: Process CF ConnectedPeer ()
readSend = do
    bs <- demandInput 2048
    loopHeader bs


loopHeader :: B.ByteString -> Process CF ConnectedPeer ()
loopHeader bs = {-# SCC "loopHeader" #-}
    let bsl = B.length bs
    in if bsl >= 4
         then let (l, r) = B.splitAt 4 bs
                  ll = readW32 l
              in if ll == 0
                    then loopHeader r -- KeepAlive
                    else loopMsg [r] (fromIntegral (B.length r)) (readW32 l)
         else do
            inp <- demandInput 2048
            loopHeader (B.concat [bs, inp]) -- We bet on this get called rarely

loopMsg :: [B.ByteString] -> Int -> Int -> Process CF ConnectedPeer ()
loopMsg lbs sz l = {-# SCC "loopMsg" #-} do
    if sz >= l
        then do let (u, r) =
                     B.splitAt (fromIntegral l)
                                (case lbs of
                                    [x] -> x
                                    rest -> (B.concat $ reverse rest))
                msg <- assert (B.length u == fromIntegral l) parseMsg l u
                c <- asks rpMsgCh
                liftIO . atomically $ writeTChan c (FromPeer (msg, fromIntegral l))
                loopHeader r
        else do inp <- demandInput 4096
                loopMsg (inp : lbs) (sz + fromIntegral (B.length inp)) l

readW32 :: B.ByteString -> Int
readW32 lbs = {-# SCC "readW32" #-}
    let [b1,b2,b3,b4] = B.unpack lbs
        b1' = fromIntegral b1
        b2' = fromIntegral b2
        b3' = fromIntegral b3
        b4' = fromIntegral b4
    in (b4' + (256 * b3') + (256 * 256 * b2') + (256 * 256 * 256 * b1'))

parseMsg :: Int -> B.ByteString -> Process CF ConnectedPeer Message
parseMsg _l u = {-# SCC "parseMsg" #-}
    case runGet decodeMsg u of
        Left err -> do warningP $ "Incorrect parse in receiver, context: " ++ show err
                       stopP
        Right msg -> return msg
