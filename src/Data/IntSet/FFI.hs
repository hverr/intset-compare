module Data.IntSet.FFI (
  IntSet
, new, free, add, check
) where

import Data.IntSet.FFI.C

import Data.Word

new :: Word64     -- ^ Minimum bound of the integer set
    -> Word64     -- ^ Maximum bound of the integer set
    -> Word64     -- ^ Size of the out-of-bounds set (must be >= 1)
    -> IO IntSet
new minB maxB s = c_new_int_set (fromIntegral minB) (fromIntegral maxB) (fromIntegral s)
{-# INLINE new #-}

free :: IntSet -> IO ()
free = c_free_int_set
{-# INLINE free #-}

add :: IntSet -> Word64 -> IO ()
add s = c_int_set_add s . fromIntegral
{-# INLINE add #-}

check :: IntSet -> Word64 -> IO Bool
check s n = (0 /=) <$> c_int_set_check s (fromIntegral n)
{-# INLINE check #-}
