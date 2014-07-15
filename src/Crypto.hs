module Crypto where

import Control.Applicative ((<$>))
import Data.Word (Word8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Habi

import Network.Socket (Socket)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Path, Test)

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

normalize :: (Integral a) => a -> a
normalize 0 = 32
normalize x = let toAdd = 32 - (x `mod` 32)
              in if toAdd == 0
                    then x + 32
                    else x + toAdd

mapE :: (e -> e') -> (a -> b) -> Either e a -> Either e' b
mapE lf _ (Left e)  = Left (lf e)
mapE _ rf (Right r) = Right (rf r)

-- TESTS


testSuite :: Test
testSuite = testGroup "Crypto"
  [ testProperty "QC normalize/multipleOf32" propMultipleOf32
  , testCase "HUnit normalize/alsoAddIfAlreadyMod32" testNormalizeAddToMultipleOf32
  ]

testNormalizeAddToMultipleOf32 :: Assertion
testNormalizeAddToMultipleOf32 = do
    assertBool "32" (normalize 32 == 64)
    assertBool "32" (normalize 64 == 96)

propMultipleOf32 :: Word8 -> Bool
propMultipleOf32 x = normalize x `mod` 32 == 0
