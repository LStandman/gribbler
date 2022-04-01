module SHA2(
    sha256_size_block,
    sha256_size_digest,
    sha256sum,
    sha256sum1)
  where

import Data.Array.Unboxed
import Data.Bits
import Data.Word

type Hash = (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32)

sha256_size_block   :: Int
sha256_size_digest  :: Int
sha256sum           :: [Word8] -> Int -> [Word8]
sha256sum1          :: [Word8] -> [Word8]

bounds_message      = (0,15)
sha256_size_block   = 64
sha256_size_digest  = 32
size_block          = 16
size_hash           = 8

ch :: Word32 -> Word32 -> Word32 -> Word32

ch x y z = (x .&. y) `xor` ((complement x) .&. z)

maj :: Word32 -> Word32 -> Word32 -> Word32

maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

big_sigma0 :: Word32 -> Word32

big_sigma0 x = (x `rotateR` 2) `xor` (x `rotateR` 13) `xor` (x `rotateR` 22)

big_sigma1 :: Word32 -> Word32

big_sigma1 x = (x `rotateR` 6) `xor` (x `rotateR` 11) `xor` (x `rotateR` 25)

lil_sigma0 :: Word32 -> Word32

lil_sigma0 x = (x `rotateR` 7) `xor` (x `rotateR` 18) `xor` (x `shiftR` 3)

lil_sigma1 :: Word32 -> Word32

lil_sigma1 x = (x `rotateR` 17) `xor` (x `rotateR` 19) `xor` (x `shiftR` 10)

sha256round :: Word32 -> Word32 -> Hash -> Hash

sha256round k w (h0, h1, h2, h3, h4, h5, h6, h7) =
  (a', b', c', d', e', f', g', h')
  where
    a = h0
    b = h1
    c = h2
    d = h3
    e = h4
    f = h5
    g = h6
    h = h7
    --
    t1 = h + (big_sigma1 e) + (ch e f g) + k + w
    t2 = (big_sigma0 a) + (maj a b c)
    h' = g
    g' = f
    f' = e
    e' = d + t1
    d' = c
    c' = b
    b' = a
    a' = t1 + t2

sha256sched' :: UArray Int Word32 -> Int -> UArray Int Word32

sha256sched' v 16 = v

sha256sched' v n = sha256sched' v' (n + 2)
  where
    v'      = v//[
      (
        i,
        (lil_sigma1 $ deref (i - 2)) + (deref (i - 7)) +
          (lil_sigma0 $ deref (i - 15)) + (deref (i - 16)))
      | i <- [n, n + 1]]
    deref j = v!((j + size_block) `mod` size_block)

sha256sched :: [Word32] -> [Word32]

sha256sched v = elems $ sha256sched' u 0
  where u = listArray bounds_message v

sha256block' :: [Word32] -> [Word32] -> Hash -> Hash

sha256block' [] [] h = h

sha256block' k w h = sha256block' k2 w2 (sha256round k1 w1 h)
  where
    k1:k2 = k
    w1:w2 = w

sha256block :: [Word32] -> [Word32] -> Hash -> Hash

sha256block k w h = (
    h0 + h0', h1 + h1', h2 + h2', h3 + h3',
    h4 + h4', h5 + h5', h6 + h6', h7 + h7')
  where
    (h0,  h1,  h2,  h3,  h4,  h5,  h6,  h7)   = h
    (h0', h1', h2', h3', h4', h5', h6', h7')  = sha256block' k w h

sha256sum' :: [Word32] -> Hash -> Hash

sha256sum' [] h = h

sha256sum' m h = sha256sum' m2 h'
  where
    k         = [
      0x428A2F98, 0x71374491, 0xB5C0FBCF, 0xE9B5DBA5,
      0x3956C25B, 0x59F111F1, 0x923F82A4, 0xAB1C5ED5,
      0xD807AA98, 0x12835B01, 0x243185BE, 0x550C7DC3,
      0x72BE5D74, 0x80DEB1FE, 0x9BDC06A7, 0xC19BF174,
      0xE49B69C1, 0xEFBE4786, 0x0FC19DC6, 0x240CA1CC,
      0x2DE92C6F, 0x4A7484AA, 0x5CB0A9DC, 0x76F988DA,
      0x983E5152, 0xA831C66D, 0xB00327C8, 0xBF597FC7,
      0xC6E00BF3, 0xD5A79147, 0x06CA6351, 0x14292967,
      0x27B70A85, 0x2E1B2138, 0x4D2C6DFC, 0x53380D13,
      0x650A7354, 0x766A0ABB, 0x81C2C92E, 0x92722C85,
      0xA2BFE8A1, 0xA81A664B, 0xC24B8B70, 0xC76C51A3,
      0xD192E819, 0xD6990624, 0xF40E3585, 0x106AA070,
      0x19A4C116, 0x1E376C08, 0x2748774C, 0x34B0BCB5,
      0x391C0CB3, 0x4ED8AA4A, 0x5B9CCA4F, 0x682E6FF3,
      0x748F82EE, 0x78A5636F, 0x84C87814, 0x8CC70208,
      0x90BEFFFA, 0xA4506CEB, 0xBEF9A3F7, 0xC67178F2]
    (m1, m2)  = splitAt size_block m
    w         = concat $ take 4 $ iterate (sha256sched) m1
    h'        = sha256block k w h

to_list :: Hash -> [Word8]

to_list (h0, h1, h2, h3, h4, h5, h6, h7) =
  concat [g w | w <- [h0, h1, h2, h3, h4, h5, h6, h7]]
  where
    g n =[
      fromIntegral i :: Word8
      | i <- [n `shiftR` 24, n `shiftR` 16, n `shiftR` 8, n]]

from_list :: [Word8] -> [Word32]

from_list [] = []

from_list m = foldl (f) 0 m1 : from_list m2
  where
    f a b     = (a `shiftL` 8) .|. fromIntegral b
    (m1, m2)  = splitAt 4 m

sha256sum m l = to_list $ sha256sum' (from_list m') h
  where
    g n     = [
      fromIntegral i :: Word8 | i <- [
        n `shiftR` 56, n `shiftR` 48, n `shiftR` 40, n `shiftR` 32,
        n `shiftR` 24, n `shiftR` 16, n `shiftR` 8, n]]
    h       = (
      0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
      0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19)
    n       = (sha256_size_block - 8 - (l + 1)) `mod` sha256_size_block
    m'      = m ++ [0x80] ++ (take n $ repeat 0) ++
      (g $ (fromIntegral l :: Word64) * 8)

sha256sum1 m = sha256sum m (length m)
