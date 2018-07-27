{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.IntSet.GHC where

import qualified Data.IntSet as CIS

import Data.Bits
import Data.Word (Word64)

import GHC.Exts (Int(..), RealWorld,
                 MutableByteArray#, newPinnedByteArray#, setByteArray#,
                 readWord64Array#, writeWord64Array#,
                 MutVar#, newMutVar#,
                 readMutVar#, writeMutVar#)
import GHC.IO (IO(IO))
import GHC.Word (Word64(..))

data IntSet = IntSet {
    intSetMinBound#  :: {-# UNPACK #-} !Word64
  , intSetMaxBound#  :: {-# UNPACK #-} !Word64
  , intSetInBounds#  :: !(MutableByteArray# RealWorld)
  , intSetOutBounds# :: !(MutVar# RealWorld CIS.IntSet)
  }


new :: Word64 -- ^ Minimum bound of the integer set
    -> Word64 -- ^ Maximum bound of the integer set
    -> IO IntSet
new !minB !maxB =
    let !numInBounds = (maxB - minB) `div` 8 + 1 in
    case fromIntegral numInBounds of { I# numInBounds# ->
    IO $ \s0 ->
        case newPinnedByteArray# numInBounds# s0 of { (# s1, mba #) ->
        case setByteArray# mba 0# numInBounds# 0# s1 of { s2 ->
        case newMutVar# CIS.empty s2 of { (# s3, mv #) ->
        (# s3, IntSet { intSetMinBound#  = minB
                      , intSetMaxBound#  = maxB
                      , intSetInBounds#  = mba
                      , intSetOutBounds# = mv
                      } #) }}}}

add :: IntSet -> Word64 -> IO ()
add !set !n =
    if n >= intSetMinBound# set && n <= intSetMaxBound# set then
        let !n' = n - intSetMinBound# set
            !i    = fromIntegral $ n' .&. 63
            !mask = (1 :: Word64) `shiftL` i
        in
        case fromIntegral $ n' `shiftR` 6 of { I# o ->
        IO $ \s0 ->
            case readWord64Array# (intSetInBounds# set) o s0 of { (# s1, b #) ->
            case W64# b .|. mask of { W64# b' ->
            case writeWord64Array# (intSetInBounds# set) o b' s1 of { s2 ->
            (# s2, () #) }}}}
    else
        IO $ \s0 ->
            case readMutVar# (intSetOutBounds# set) s0 of { (# s1, m #) ->
            let !m' = CIS.insert (fromIntegral n) m in
            case writeMutVar# (intSetOutBounds# set) m' s1 of { s2 ->
            (# s2, () #) }}

check :: IntSet -> Word64 -> IO Bool
check !set !n =
    if n >= intSetMinBound# set && n <= intSetMaxBound# set then
        let !n' = n - intSetMinBound# set
            !i    = fromIntegral $ n' .&. 63
            !mask = (1 :: Word64) `shiftL` i
        in
        case fromIntegral $ n' `shiftR` 6 of { I# o ->
        IO $ \s0 ->
            case readWord64Array# (intSetInBounds# set) o s0 of { (# s1, b #) ->
            let !f = (W64# b .&. mask) /= 0 in
            (# s1, f #) }}
    else
        IO $ \s0 ->
            case readMutVar# (intSetOutBounds# set) s0 of { (# s1, m #) ->
            let !f = CIS.member (fromIntegral n) m in
            (# s1, f #) }
