{-# LANGUAGE OverloadedStrings #-}
module Crypto where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (MVar, newMVar, modifyMVar)
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

data ConnectedPeer = ConnectedPeer {
      cpSocket  :: Socket     -- ^ connection to peer
    , cpSessKey :: SessionKey -- ^ key for symmetric encryption
    , cpIVMVarE  :: (MVar IV)  -- ^ initialization vector for encryption
    , cpIVMVarD  :: (MVar IV)  -- ^ initialization vector for decryption
}

data Type = Leecher | Seeder

data CryptoCtx = CryptoCtx { _homedir :: String, _fpr :: BS.ByteString }

handshakeSeeder :: Socket -> CryptoCtx -> IO (Either String ConnectedPeer)
handshakeSeeder = runHS Seeder seederHandshake

handshakeLeecher :: Socket -> CryptoCtx -> IO (Either String ConnectedPeer)
handshakeLeecher = runHS Leecher leecherHandshake

runHS :: Type -> (Socket -> String -> Fpr -> IO (Either Error SessionKey))
       -> Socket -> CryptoCtx -> IO (Either String ConnectedPeer)
runHS t hs s (CryptoCtx h fpr) = do
    eSessK <- hs s h fpr
    case eSessK of
        Left err -> return (Left $ show err)
        Right sessK -> Right <$> mkConnPeer t s sessK

mkConnPeer :: Type -> Socket -> SessionKey -> IO ConnectedPeer
mkConnPeer t s k = ConnectedPeer s k <$> mkIVMVarE t <*> mkIVMVarD t
  where mkIVMVarE Seeder  = newMVar (incrementIV newIV)
        mkIVMVarE Leecher = newMVar newIV
        mkIVMVarD Leecher = newMVar (incrementIV newIV)
        mkIVMVarD Seeder  = newMVar newIV
    
encryptL :: ConnectedPeer -> LBS.ByteString -> IO LBS.ByteString
encryptL (ConnectedPeer _ key ivVar _) plain = do
    iv <- modifyMVar ivVar incIV
    cipher <- symmetricEncrypt key iv (LBS.toStrict plain)
    return (either error LBS.fromStrict cipher)

decrypt :: ConnectedPeer -> BS.ByteString -> IO BS.ByteString
decrypt (ConnectedPeer _ key _ ivVar) cipher = do
    iv <- modifyMVar ivVar incIV
    plain <- symmetricDecrypt key iv cipher
    return (either error id plain)

incIV :: IV -> IO (IV, IV)
incIV iv = return (incTwice iv, incTwice iv)
    where incTwice = incrementIV . incrementIV


normalize :: (Integral a) => a -> a
normalize 0 = 32
normalize x = let toAdd = 32 - (x `mod` 32)
              in x + (if toAdd == 0 then 32 else toAdd)

mapE :: (e -> e') -> (a -> b) -> Either e a -> Either e' b
mapE lf _ (Left e)  = Left (lf e)
mapE _ rf (Right r) = Right (rf r)

-- TESTS


testSuite :: Test
testSuite = testGroup "Crypto"
  [ testProperty "QC normalize/multipleOf32" propMultipleOf32
  , testCase "HUnit normalize/alsoAddIfAlreadyMod32" testNormalizeAddToMultipleOf32
  , testCase "HUnit symEncrypion/contToWork" testSymmEncryptionCont
  , testCase "HUnit symEncrypion/bothWays" testSymmEncryptionBothWays
  , testCase "HUnit symEncrypion/changeIV" testSymmEncryptionIV
  , testCase "HUnit symEncrypion/distinctIV" testSymmEncryptionDistinctIV
  ]

testNormalizeAddToMultipleOf32 :: Assertion
testNormalizeAddToMultipleOf32 =
    mapM_ (\(x, y) -> assertBool (show x) (normalize x == y)) vals
 where vals = [ (x, x+norm) | x <- [0::Int, norm ..(10::Int) * norm]]
       norm :: Int
       norm = 32

testSymmEncryptionCont :: Assertion
testSymmEncryptionCont = do
    sessKey <- randomSessionKey
    p1 <- mkConnPeer Leecher undefined sessKey 
    p2 <- mkConnPeer Seeder undefined sessKey 

    cipher1 <- encryptL p1 "abc"
    plain1 <- decrypt p2 $ LBS.toStrict cipher1
    assertEqual "should decrypt to same" "abc" plain1

    cipher2 <- encryptL p1 "def"
    plain2 <- decrypt p2 $ LBS.toStrict cipher2
    assertEqual "should decrypt to same" "def" plain2

-- first a then b
testSymmEncryptionBothWays :: Assertion
testSymmEncryptionBothWays = do
    sessKey <- randomSessionKey
    p1 <- mkConnPeer Leecher undefined sessKey 
    p2 <- mkConnPeer Seeder undefined sessKey 

    cipher1 <- encryptL p1 "abc"
    plain1 <- decrypt p2 $ LBS.toStrict cipher1
    assertEqual "should decrypt to same" "abc" plain1

    cipher2 <- encryptL p2 "def"
    plain2 <- decrypt p1 $ LBS.toStrict cipher2
    assertEqual "should decrypt to same" "def" plain2

-- iv must change
testSymmEncryptionIV :: Assertion
testSymmEncryptionIV = do
    sessKey <- randomSessionKey
    p1 <- mkConnPeer Leecher undefined sessKey 

    cipher1 <- encryptL p1 "abc"

    cipher2 <- encryptL p1 "abc"
    assertBool "should encrypt to different ciphers" (cipher1 /= cipher2)

-- the first message of each needs distinct iv
testSymmEncryptionDistinctIV :: Assertion
testSymmEncryptionDistinctIV = do
    sessKey <- randomSessionKey
    p1 <- mkConnPeer Leecher undefined sessKey 
    p2 <- mkConnPeer Seeder undefined sessKey 

    cipher1 <- encryptL p1 "abc"
    cipher2 <- encryptL p2 "abc"

    assertBool "should decrypt to same" (cipher1 /= cipher2)
    

propMultipleOf32 :: Word8 -> Bool
propMultipleOf32 x = normalize x `mod` 32 == 0
