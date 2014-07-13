module Crypto where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Socket (Socket)

type Fpr = BS.ByteString
type SessionKey = BS.ByteString
data ConnectedPeer = ConnectedPeer Socket SessionKey

data CryptoCtx = CryptoCtx { _homedir :: String, _fpr :: BS.ByteString }

handshakeSeeder :: CryptoCtx -> IO ConnectedPeer
handshakeSeeder = undefined

handshakeLeecher :: CryptoCtx -> IO ConnectedPeer
handshakeLeecher = undefined

encrypt :: ConnectedPeer -> BS.ByteString -> BS.ByteString
encrypt = undefined

encryptL :: ConnectedPeer -> LBS.ByteString -> LBS.ByteString
encryptL = undefined

decrypt :: ConnectedPeer -> BS.ByteString -> BS.ByteString
decrypt = undefined

decryptL :: ConnectedPeer -> LBS.ByteString -> LBS.ByteString
decryptL = undefined
