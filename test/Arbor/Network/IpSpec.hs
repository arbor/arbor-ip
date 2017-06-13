module Arbor.Network.IpSpec
where

import Arbor.Network.Ip
import Data.Semigroup
import Data.Word
import Test.Hspec

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

ipOctets :: Monad m => Gen m (Word8, Word8, Word8, Word8)
ipOctets =
  let w8 = Gen.word8 Range.constantBounded
  in (,,,) <$> w8 <*> w8 <*> w8 <*> w8

spec :: Spec
spec = describe "App.IpSpec" $ do
  it "IPv4 octets roundtrip" . require . property $ do
    os <- forAll ipOctets
    ipToOctets (ipFromOctets' os) === os

  it "should encode IPv4 block as CIDR" $ do
    rangeToString (IPv4Range (IPv4 12) (IPv4BitPower 0)) `shouldBe` "0.0.0.12/32"
    rangeToString (IPv4Range (IPv4 12) (IPv4BitPower 8)) `shouldBe` "0.0.0.12/24"

  it "should return last IP in range" $ do
    rangeLast (IPv4Range (IPv4 16) (IPv4BitPower 1)) `shouldBe` IPv4 17

  it "should format string IP" . require . property $ do
    (a, b, c, d) <- forAll ipOctets
    ipToString (ipFromOctets a b c d) === show a <> "." <> show b <> "." <> show c <> "." <> show d
