{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HW6.T1
  ( Bucket
  , BucketsArray
  , CHT(..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  ) where

import Control.Concurrent.Classy
import Data.Array.MArray
import Data.Hashable (Hashable, hash)
import Control.Monad (when, forM_)
import Data.Maybe (isNothing)

-- Константы
initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.7

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

getBucketIndex :: Hashable k => Int -> k -> Int
getBucketIndex capacity key = hash key `mod` capacity

newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  buckets <- newArray (0, initCapacity - 1) []
  bucketsTVar <- newTVar buckets
  sizeTVar <- newTVar 0
  return $ CHT bucketsTVar sizeTVar

getCHT :: (MonadConc m, Eq k, Hashable k) => k -> CHT (STM m) k v -> m (Maybe v)
getCHT key cht = atomically $ do
  bucketsArray <- readTVar (chtBuckets cht)
  (lo, hi) <- getBounds bucketsArray
  let capacity = hi - lo + 1
      idx = getBucketIndex capacity key
  bucket <- readArray bucketsArray idx
  return $ lookup key bucket

putCHT :: forall m k v. (MonadConc m, Eq k, Hashable k) => k -> v -> CHT (STM m) k v -> m ()
putCHT key value cht = atomically $ do
  bucketsArray <- readTVar (chtBuckets cht)
  size <- readTVar (chtSize cht)
  (lo, hi) <- getBounds bucketsArray
  let capacity = hi - lo + 1
      idx = getBucketIndex capacity key
  bucket <- readArray bucketsArray idx
  let (existing, others) = partitionByKey key bucket
      newBucket = (key, value) : others
      sizeDelta = if isNothing existing then 1 else 0

  writeArray bucketsArray idx newBucket
  let newSize = size + sizeDelta
  writeTVar (chtSize cht) newSize

  when (newSize >= floor (fromIntegral capacity * loadFactor)) $
    resizeCHT @m cht

resizeCHT :: forall m k v. (MonadConc m, Eq k, Hashable k) => CHT (STM m) k v -> STM m ()
resizeCHT cht = do
  oldBucketsArray <- readTVar (chtBuckets cht)
  (lo, hi) <- getBounds oldBucketsArray
  let oldCapacity = hi - lo + 1
      newCapacity = oldCapacity * 2

  newBucketsArray <- newArray (0, newCapacity - 1) []

  forM_ [lo .. hi] $ \i -> do
    bucket <- readArray oldBucketsArray i
    forM_ bucket $ \(k, v) -> do
      let idx = getBucketIndex newCapacity k
      newBucket <- readArray newBucketsArray idx
      writeArray newBucketsArray idx ((k, v) : newBucket)

  writeTVar (chtBuckets cht) newBucketsArray

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT cht = readTVarConc (chtSize cht)

partitionByKey :: Eq k => k -> [(k, v)] -> (Maybe v, [(k, v)])
partitionByKey _ [] = (Nothing, [])
partitionByKey key ((k, v) : xs)
  | key == k  = (Just v, xs)
  | otherwise = let (found, rest) = partitionByKey key xs in (found, (k, v) : rest)
