{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive

import Data.HashSet (HashSet)
import Data.Word
import qualified Data.HashSet as HS

import qualified Data.IntSet.FFI as FFI
import qualified Data.IntSet.Native as Native

import System.Random

main :: IO ()
main = do
    putStrLn "[1] testOnceFFI"
    testOnceFFI
    putStrLn "[1] testOnceNative"
    testOnceNative

testOnceFFI :: IO ()
testOnceFFI = do
    minB <- randomRIO (minBound, maxBound - 1000*1000*1000)
    maxB <- randomRIO (minB + 1, minB + 10*1000)

    let newRand :: IO Word64
        newRand = do
            f <- randomIO
            if f then randomIO else randomRIO (minB, maxB)

    bracket (FFI.new minB maxB 100)
            FFI.free
            (doAll 10000 HS.empty newRand)

    putStrLn "Success"
  where
    doAll :: Int -> HashSet Word64 -> IO Word64 -> FFI.IntSet -> IO ()
    doAll !k !s' newRand s
        | k <= 0 = return ()
        | otherwise = do
            forM_ (HS.toList s') $ \n -> do
                f <- FFI.check s n
                unless f $
                    throwIO $ TestError "added number did not return true"

            replicateM_ 100 $ do
                n <- randomIO
                f <- FFI.check s n
                let g = HS.member n s'
                when (g /= f) $
                    throwIO $ TestError "random number did not match"

            n <- newRand
            FFI.add s n
            doAll (k-1) (HS.insert n s') newRand s

testOnceNative :: IO ()
testOnceNative = do
    minB <- randomRIO (minBound, maxBound - 1000*1000*1000)
    maxB <- randomRIO (minB + 1, minB + 10*1000)

    let newRand :: IO Word64
        newRand = do
            f <- randomIO
            if f then randomIO else randomRIO (minB, maxB)

    set <- Native.new minB maxB
    doAll 10000 HS.empty newRand set

    putStrLn "Success"
  where
    doAll :: Int -> HashSet Word64 -> IO Word64 -> Native.IntSet (PrimState IO) -> IO ()
    doAll !k !s' newRand s
        | k <= 0 = return ()
        | otherwise = do
            forM_ (HS.toList s') $ \n -> do
                f <- Native.check s n
                unless f $
                    throwIO $ TestError "added number did not return true"

            replicateM_ 100 $ do
                n <- randomIO
                f <- Native.check s n
                let g = HS.member n s'
                when (g /= f) $
                    throwIO $ TestError "random number did not match"

            n <- newRand
            Native.add s n
            doAll (k-1) (HS.insert n s') newRand s

data TestError = TestError String deriving (Show)

instance Exception TestError
