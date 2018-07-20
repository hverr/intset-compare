{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.IntSet.Native (
  -- * Types
  IntSet

  -- * Allocation
, new

  -- * Elements
, add
, check
) where

import Control.Monad.Primitive (PrimMonad(..))

import Data.Bits
import Data.Primitive.ByteArray
import Data.Primitive.MutVar
import Data.Word
import qualified Data.IntSet as CIS

data IntSet s = IntSet {
    intSetMinBound#  :: {-# UNPACK #-} !Word64
  , intSetMaxBound#  :: {-# UNPACK #-} !Word64
  , intSetInBounds#  :: {-# UNPACK #-} !(MutableByteArray s)
  , intSetOutBounds# :: {-# UNPACK #-} !(MutVar s CIS.IntSet)
  }

new :: PrimMonad m
    => Word64 -- ^ Minimum bound of the integer set
    -> Word64 -- ^ Maximum bound of the integer set
    -> m (IntSet (PrimState m))
new minB maxB = do
    let !numInBounds = (maxB - minB) `div` 8 + 1
    set <- newByteArray (fromIntegral numInBounds)
    fillByteArray set 0 (fromIntegral numInBounds) 0
    outBounds <- newMutVar CIS.empty
    return $! IntSet {
        intSetMinBound#  = minB
      , intSetMaxBound#  = maxB
      , intSetInBounds#  = set
      , intSetOutBounds# = outBounds
      }

add :: PrimMonad m => IntSet (PrimState m) -> Word64 -> m ()
add set n =
    if n >= intSetMinBound# set && n <= intSetMaxBound# set then do
        let !n' = n - intSetMinBound# set
        let !o    = fromIntegral $ n' `div` 64
        let !i    = fromIntegral $ n' `rem` 64
        let !mask = (1 :: Word64) `shiftL` i
        b <- readByteArray (intSetInBounds# set) o
        let !b' = b .|. mask
        writeByteArray (intSetInBounds# set) o b'

    else do
        m <- readMutVar (intSetOutBounds# set)
        let !m' = CIS.insert (fromIntegral n) m
        writeMutVar (intSetOutBounds# set) m'

check :: PrimMonad m => IntSet (PrimState m) -> Word64 -> m Bool
check set n =
    if n >= intSetMinBound# set && n <= intSetMaxBound# set then do
        let !n' = n - intSetMinBound# set
        let !o    = fromIntegral $ n' `div` 64
        let !i    = fromIntegral $ n' `rem` 64
        let !mask = (1 :: Word64) `shiftL` i
        b <- readByteArray (intSetInBounds# set) o
        return $! (b .&. mask) /= 0

    else do
        m <- readMutVar (intSetOutBounds# set)
        return $! CIS.member (fromIntegral n) m
