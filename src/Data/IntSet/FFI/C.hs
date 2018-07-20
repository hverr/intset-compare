{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.IntSet.FFI.C where

import Foreign.C.Types
import Foreign.Ptr (Ptr)

data IntSetPtr

type IntSet = Ptr IntSetPtr

foreign import ccall unsafe "new_int_set" c_new_int_set ::
    CULong      -- ^ Minimum bound of the integer set
 -> CULong      -- ^ Maximum bound of the integer set
 -> CSize       -- ^ Size of the out-of-bounds set (must be >= 1)
 -> IO IntSet

foreign import ccall unsafe "free_int_set" c_free_int_set ::
    IntSet
 -> IO ()

foreign import ccall unsafe "int_set_add" c_int_set_add ::
    IntSet
 -> CULong
 -> IO ()

foreign import ccall unsafe "int_set_check" c_int_set_check ::
    IntSet
 -> CULong
 -> IO CBool
