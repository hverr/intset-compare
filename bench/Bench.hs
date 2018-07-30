import Control.Exception

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict

import Criterion.Main

import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.HashSet as HashSet
import qualified Data.IntSet as Containers
import qualified Data.IntSet.FFI as FFI
import qualified Data.IntSet.GHC as ISGHC
import qualified Data.IntSet.Native as Native
import qualified Data.IntSet.NativeDiv as NativeDiv
import qualified Data.Set as Set

import System.Random

main :: IO ()
main = defaultMain [
    bgroup "ffi" [ let n = 1000;         !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetFFI 0 n v)
                 , let n = 10*1000;      !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetFFI 0 n v)
                 , let n = 100*1000;     !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetFFI 0 n v)
                 , let n = 1000*1000;    !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetFFI 0 n v)
                 , let n = 10*1000*1000; !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetFFI 0 n v)
                 ]
  , bgroup "native" [ let n = 1000;         !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetNative 0 n v)
                    , let n = 10*1000;      !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetNative 0 n v)
                    , let n = 100*1000;     !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetNative 0 n v)
                    , let n = 1000*1000;    !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetNative 0 n v)
                    , let n = 10*1000*1000; !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetNative 0 n v)
                    ]
  , bgroup "native-div" [ let n = 1000;         !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetNativeDiv 0 n v)
                        , let n = 10*1000;      !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetNativeDiv 0 n v)
                        , let n = 100*1000;     !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetNativeDiv 0 n v)
                        , let n = 1000*1000;    !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetNativeDiv 0 n v)
                        , let n = 10*1000*1000; !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetNativeDiv 0 n v)
                        ]
  , bgroup "ghc" [ let n = 1000;         !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetGHC 0 n v)
                 , let n = 10*1000;      !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetGHC 0 n v)
                 , let n = 100*1000;     !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetGHC 0 n v)
                 , let n = 1000*1000;    !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetGHC 0 n v)
                 , let n = 10*1000*1000; !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (intsetGHC 0 n v)
                 ]
  , bgroup "containers" [ let n = 1000;         !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetContainers v)
                        , let n = 10*1000;      !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetContainers v)
                        , let n = 100*1000;     !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetContainers v)
                        , let n = 1000*1000;    !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetContainers v)
                        , let n = 10*1000*1000; !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetContainers v)
                        ]
  , bgroup "set" [ let n = 1000;         !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetSet v)
                 , let n = 10*1000;      !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetSet v)
                 , let n = 100*1000;     !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetSet v)
                 , let n = 1000*1000;    !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetSet v)
                 , let n = 10*1000*1000; !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetSet v)
                 ]
  , bgroup "hashset" [ let n = 1000;         !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetHashSet v)
                     , let n = 10*1000;      !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetHashSet v)
                     , let n = 100*1000;     !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetHashSet v)
                     , let n = 1000*1000;    !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetHashSet v)
                     , let n = 10*1000*1000; !v = generateInts 0 n (fromIntegral n) in bench (show n) $ whnfIO (intsetHashSet v)
                     ]
  ]

intsetFFI :: Word64
          -> Word64
          -> Vector Word64
          -> IO ()
intsetFFI minB maxB xs = bracket (FFI.new minB maxB 100) FFI.free $ \s ->
    forM_ xs $ \i -> do
        FFI.add s i
        f <- FFI.check s i
        unless f $
            throwIO $ userError "implementation errors"

intsetNativeDiv :: Word64
                -> Word64
                -> Vector Word64
                -> IO ()
intsetNativeDiv minB maxB xs = do
    s <- NativeDiv.new minB maxB
    forM_ xs $ \i -> do
        NativeDiv.add s i
        f <- NativeDiv.check s i
        unless f $
            throwIO $ userError "implementation errors"

intsetNative :: Word64
             -> Word64
             -> Vector Word64
             -> IO ()
intsetNative minB maxB xs = do
    s <- Native.new minB maxB
    forM_ xs $ \i -> do
        Native.add s i
        f <- Native.check s i
        unless f $
            throwIO $ userError "implementation errors"

intsetGHC :: Word64
          -> Word64
          -> Vector Word64
          -> IO ()
intsetGHC minB maxB xs = do
    s <- ISGHC.new minB maxB
    forM_ xs $ \i -> do
        ISGHC.add s i
        f <- ISGHC.check s i
        unless f $
            throwIO $ userError "implementation errors"

intsetContainers :: Vector Word64 -> IO Containers.IntSet
intsetContainers xs = execStateT go Containers.empty
  where
    go = forM xs $ \i -> do
        modify' $ Containers.insert (fromIntegral i)
        f <- gets $ Containers.member (fromIntegral i)
        unless f $
            liftIO . throwIO $ userError "implementation errors"

intsetSet :: Vector Word64 -> IO (Set.Set Word64)
intsetSet xs = execStateT go Set.empty
  where
    go = forM xs $ \i -> do
        modify' $ Set.insert i
        f <- gets $ Set.member i
        unless f $
            liftIO . throwIO $ userError "implementation errors"

intsetHashSet :: Vector Word64 -> IO (HashSet.HashSet Word64)
intsetHashSet xs = execStateT go HashSet.empty
  where
    go = forM xs $ \i -> do
        modify' $ HashSet.insert i
        f <- gets $ HashSet.member i
        unless f $
            liftIO . throwIO $ userError "implementation errors"

generateInts :: Word64
             -> Word64
             -> Int
             -> Vector Word64
generateInts minB maxB n =
    V.fromList . take n $ randomRs (minB, maxB) (mkStdGen 0x214f36c9)
