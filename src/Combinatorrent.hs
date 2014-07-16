module Main (main)
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State

import Data.Maybe
import Data.List

import System.Environment
import System.Random

import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.FilePath ()
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO as SIO

import qualified Process.Console as Console
import qualified Process.PeerMgr as PeerMgr
import qualified Process.ChokeMgr as ChokeMgr (start)
import qualified Process.Listen as Listen
import qualified Process.DirWatcher as DirWatcher (start)
import qualified Process.Status as Status (start, StatusChannel, PStat)
import qualified Process.TorrentManager as TorrentManager (start, TorrentMgrChan, TorrentManagerMsg(..))

import qualified Data.ByteString.Char8 as Char8

import Crypto
import Supervisor
import Torrent
import Version
import qualified Test

main :: IO ()
main = do args <- getArgs
          if "--tests" `elem` args
              then Test.runTests
              else progOpts args >>= run

-- COMMAND LINE PARSING

data Flag = Version
          | Debug
          | LogFile FilePath
          | WatchDir FilePath
          | StatFile FilePath
          | PortArg String
          | GpgHomedir FilePath
          | GpgFingerprint String
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option "V?"           ["version"] (NoArg Version)         "Show version number"
  , Option "D"            ["debug"]   (NoArg Debug)           "Spew extra debug information"
  , Option ""             ["logfile"] (ReqArg LogFile "FILE") "Choose a filepath on which to log"
  , Option "W"            ["watchdir"] (ReqArg WatchDir "DIR") "Choose a directory to watch for torrents"
  , Option "S"            ["statfile"] (ReqArg StatFile "FILE") "Choose a file to gather stats into"
  , Option "P"            ["port"]    (ReqArg PortArg "PORT") "Chose a port to listen on"
  , Option "H"            ["homedir"] (ReqArg GpgHomedir "DIR") "Specify the GPG Homedir"
  , Option "F"            ["fingerprint"] (ReqArg GpgFingerprint "DIR") "The fingerprint to use"
  ]

(~=) :: Flag -> Flag -> Bool
Version ~= Version = True
Debug ~= Debug = True
LogFile _ ~= LogFile _ = True
WatchDir _ ~= WatchDir _ = True
StatFile _ ~= StatFile _ = True
PortArg _ ~= PortArg _ = True
GpgHomedir _ ~= GpgHomedir _ = True
GpgFingerprint _ ~= GpgFingerprint _ = True
_ ~= _ = False

flag :: Flag -> [Flag] -> Maybe Flag
flag x = find (x ~=)

progOpts :: [String] -> IO ([Flag], [String])
progOpts args =
    case getOpt Permute options args of
        (o,n,[]  ) -> return (o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: Combinatorrent [OPTION...] file"

run :: ([Flag], [String]) -> IO ()
run (flags, files) =
    if Version `elem` flags
        then progHeader
        else case files of
                [] | isNothing $ flag (WatchDir "") flags -> putStrLn "No torrentfile input"
                names -> progHeader >> download flags names

progHeader :: IO ()
progHeader = putStrLn $ "This is Combinatorrent \x2620 version " ++ version ++ "\n" ++
                        "  For help type 'help'\n"

setupLogging :: [Flag] -> IO ()
setupLogging flags = do
    fLog <- case flag (LogFile "") flags of
                Nothing -> streamHandler SIO.stdout DEBUG
                Just (LogFile fp) -> fileHandler fp DEBUG
                Just _ -> error "Impossible match"
    when (Debug `elem` flags)
          (updateGlobalLogger rootLoggerName
                 (setHandlers [fLog] . setLevel DEBUG))

setupDirWatching :: [Flag] -> TorrentManager.TorrentMgrChan -> IO [Child]
setupDirWatching flags watchC =
    case flag (WatchDir "") flags of
        Nothing -> return []
        Just (WatchDir dir) -> do
            ex <- doesDirectoryExist dir
            if ex
                then return [ Worker $ DirWatcher.start dir watchC ]
                else do putStrLn "Directory does not exist, not watching"
                        return []
        Just _ -> error "Impossible match"

setupStatus :: [Flag] -> Status.StatusChannel -> TVar [Status.PStat] -> Child
setupStatus flags statusC stv =
    case flag (StatFile "") flags of
      Nothing -> Worker $ Status.start Nothing statusC stv
      Just (StatFile fn) -> Worker $ Status.start (Just fn) statusC stv
      Just _ -> error "Impossible match"

generatePeerId :: IO MyPeerId
generatePeerId = do
    gen <- getStdGen
    return $ mkPeerId gen

cryptoSetup :: [Flag] -> IO CryptoCtx
cryptoSetup flags = do
    let homedir = case flag (GpgHomedir "") flags of
                    Nothing -> error "GpgHomedir is required"
                    Just (GpgHomedir dir) -> dir
                    Just _ -> error "Impossible match"
    let fpr = case flag (GpgFingerprint "") flags of
                Nothing -> error "GpgFingerprint is required"
                Just (GpgFingerprint f) -> f
                Just _ -> error "Impossible match"
    valid <- doesDirectoryExist homedir
    unless valid (error "GpgHomedir must exist")
    return $ CryptoCtx homedir (Char8.pack fpr)

getPort :: [Flag] -> Port
getPort flags =
    maybe undefined (\(PortArg p) -> P $ read p) $ flag (PortArg undefined) flags

download :: [Flag] -> [String] -> IO ()
download flags names = do
    cryptoCtx <- cryptoSetup flags
    let port = getPort flags
    setupLogging flags
    watchC <- liftIO newTChanIO
    workersWatch <- setupDirWatching flags watchC
    -- setup channels
    statusC  <- liftIO newTChanIO
    waitC    <- liftIO newEmptyTMVarIO
    supC <- liftIO newTChanIO
    pmC <- liftIO newTChanIO
    chokeC <- liftIO newTChanIO
    rtv <- atomically $ newTVar []
    stv <- atomically $ newTVar []
    debugM "Main" "Created channels"
    pid <- generatePeerId
    (tid, _) <- allForOne "MainSup"
              (workersWatch ++
              [ Worker $ Console.start waitC statusC
              , Worker $ TorrentManager.start port watchC statusC stv chokeC pid pmC
              , setupStatus flags statusC stv
              , Worker $ PeerMgr.start port cryptoCtx pmC pid chokeC rtv
              , Worker $ ChokeMgr.start chokeC rtv 100 -- 100 is upload rate in KB
              , Worker $ Listen.start port pmC
              ]) supC
    atomically $ writeTChan watchC (map TorrentManager.AddedTorrent names)
    _ <- atomically $ takeTMVar waitC
    infoM "Main" "Closing down, giving processes 10 seconds to cool off"
    atomically $ writeTChan supC (PleaseDie tid)
    threadDelay $ 10*1000000
    infoM "Main" "Done..."
    return ()

