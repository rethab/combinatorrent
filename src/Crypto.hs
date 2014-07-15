module Crypto where

import Control.Applicative ((<$>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Habi

import Network.Socket (Socket)

type Fpr = BS.ByteString
type SessionKey = BS.ByteString
data ConnectedPeer = ConnectedPeer Socket SessionKey

data CryptoCtx = CryptoCtx { _homedir :: String, _fpr :: BS.ByteString }

handshakeSeeder :: Socket -> CryptoCtx -> IO (Either String ConnectedPeer)
handshakeSeeder s (CryptoCtx h fpr) =
    mapE show (ConnectedPeer s) <$> seederHandshake s h fpr

handshakeLeecher :: Socket -> CryptoCtx -> IO (Either String ConnectedPeer)
handshakeLeecher s (CryptoCtx h fpr) = 
    mapE show (ConnectedPeer s) <$> leecherHandshake s h fpr

encryptL :: ConnectedPeer -> LBS.ByteString -> IO LBS.ByteString
encryptL (ConnectedPeer _ key) plain =
    either error LBS.fromStrict <$> symmetricEncrypt key newIV (LBS.toStrict plain)

decrypt :: ConnectedPeer -> BS.ByteString -> IO BS.ByteString
decrypt (ConnectedPeer _ key) cipher =
    either error id <$> symmetricDecrypt key newIV cipher

mapE :: (e -> e') -> (a -> b) -> Either e a -> Either e' b
mapE lf _ (Left e)  = Left (lf e)
mapE _ rf (Right r) = Right (rf r)
