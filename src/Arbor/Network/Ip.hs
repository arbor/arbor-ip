{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Arbor.Network.Ip
( IPv4(..), IPv4BitPower(..), IPv4Range(..)
, ranges, rangeSize, rangeLast
, ipToOctets, ipFromOctets, ipFromOctets'
, ipToString, rangeToString, ipToWord32
, rangeFrom, rangeFrom'
) where

import Data.Bits
import Data.List
import Data.Monoid
import Data.Word

newtype IPv4BitPower = IPv4BitPower Word32 deriving (Eq, Show, Ord)
newtype IPv4 = IPv4 Word32 deriving (Eq, Show, Ord, Enum, Bounded)

data IPv4Range = IPv4Range
  { rangeBase     :: !IPv4
  , rangeBitPower :: !IPv4BitPower
  } deriving (Eq, Show, Ord)

instance Bounded IPv4BitPower where
  minBound = IPv4BitPower 0
  maxBound = IPv4BitPower 31

rangeSize :: IPv4Range -> Word32
rangeSize (IPv4Range _ (IPv4BitPower b)) =
  1 `shiftL` fromIntegral b

ipToOctets :: IPv4 -> (Word8, Word8, Word8, Word8)
ipToOctets (IPv4 w) =
    let a =  fromIntegral $  w              .&. 0xFF
        b =  fromIntegral $ (w `shiftR`  8) .&. 0xFF
        c =  fromIntegral $ (w `shiftR` 16) .&. 0xFF
        d =  fromIntegral $ (w `shiftR` 24) .&. 0xFF
    in (d, c, b, a)

ipFromOctets' :: (Word8, Word8, Word8, Word8) -> IPv4
ipFromOctets' (a, b, c ,d) = ipFromOctets a b c d

ipFromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> IPv4
ipFromOctets a b c d =
    let w =  (fromIntegral a `shiftL` 24)
            .|. (fromIntegral b `shiftL` 16)
            .|. (fromIntegral c `shiftL`  8)
            .|. fromIntegral  d
    in IPv4 w

ranges :: IPv4 -> IPv4 -> [IPv4Range]
ranges startInclusive stopInclusive =
  let isRange (s, e, _) = s /= e
      allRanges = iterate (\x -> splitRange (fst' x) (snd' x)) (splitRange startInclusive (succ stopInclusive))
  in sort $ trd' <$> takeWhileInclusive isRange allRanges

rangeLast :: IPv4Range -> IPv4
rangeLast (IPv4Range (IPv4 w) (IPv4BitPower b)) =
  IPv4 $ w + (2^b - 1)

ipToString :: IPv4 -> String
ipToString ip =
  let (a, b, c, d) = ipToOctets ip
   in mconcat . intersperse "." $ show <$> [a,b,c,d]

ipToWord32 :: IPv4 -> Word32
ipToWord32 (IPv4 w) = w
{-# INLINE ipToWord32 #-}

rangeToString :: IPv4Range -> String
rangeToString (IPv4Range ip (IPv4BitPower b)) =
  ipToString ip <> "/" <> show (32-b)

rangeFrom :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Maybe IPv4Range
rangeFrom a b c d m =
  if m < 1 || m > 32
    then Nothing
    else Just $ IPv4Range (ipFromOctets a b c d) (IPv4BitPower (32 - fromIntegral m))

rangeFrom' :: [Word8] -> Maybe IPv4Range
rangeFrom' [a,b,c,d,m] = rangeFrom a b c d m
rangeFrom' _           = Nothing
-------------------------------------------------------------------------------

type SplittedRange = (IPv4, IPv4, IPv4Range)

fst' :: SplittedRange -> IPv4
fst' (a, _, _) = a

snd' :: SplittedRange -> IPv4
snd' (_, b, _) = b

trd' :: SplittedRange -> IPv4Range
trd' (_, _, r) = r

splitRange :: IPv4 -> IPv4 -> SplittedRange
splitRange (IPv4 start) (IPv4 end) =
  if startRadix <= stopRadix
    then (IPv4 (start + minBlockSize), IPv4 end, IPv4Range (IPv4 start) (IPv4BitPower minRadix))
    else (IPv4 start, IPv4(end-minBlockSize), IPv4Range (IPv4 (end-minBlockSize)) (IPv4BitPower minRadix))
  where
    lowestPowerOfTwo x = if x == 0 then 0 else fromIntegral (countTrailingZeros x)
    startRadix = lowestPowerOfTwo start
    stopRadix = lowestPowerOfTwo end
    minRadix = min startRadix stopRadix
    minBlockSize = 2 ^ minRadix

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) =
  x : if p x then takeWhileInclusive p xs else []
