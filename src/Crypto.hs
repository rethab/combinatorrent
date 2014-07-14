module Crypto where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Habi as Habi
import Network.Socket (Socket)

type Fpr = BS.ByteString
type SessionKey = BS.ByteString
data ConnectedPeer = ConnectedPeer Socket SessionKey

data CryptoCtx = CryptoCtx { _homedir :: String, _fpr :: BS.ByteString }

handshakeSeeder :: Socket -> CryptoCtx -> IO (Either String ConnectedPeer)
handshakeSeeder s (CryptoCtx h fpr) =
    mapE show (ConnectedPeer s) `fmap` Habi.seederHandshake s h fpr

handshakeLeecher :: Socket -> CryptoCtx -> IO (Either String ConnectedPeer)
handshakeLeecher s (CryptoCtx h fpr) = 
    mapE show (ConnectedPeer s) `fmap` Habi.leecherHandshake s h fpr

encrypt :: ConnectedPeer -> BS.ByteString -> BS.ByteString
encrypt = undefined

encryptL :: ConnectedPeer -> LBS.ByteString -> LBS.ByteString
encryptL = undefined

decrypt :: ConnectedPeer -> BS.ByteString -> BS.ByteString
decrypt = undefined

decryptL :: ConnectedPeer -> LBS.ByteString -> LBS.ByteString
decryptL = undefined

mapE :: (e -> e') -> (a -> b) -> Either e a -> Either e' b
mapE lf _ (Left e)  = Left (lf e)
mapE _ rf (Right r) = Right (rf r)
