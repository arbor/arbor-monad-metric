{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.Counter
  ( MonadMetric
  , Z.getMetrics

  , incByKey
  , incByKey'
  , addByKey
  , addByKey'
  , setByKey
  , setByKey'

  , newMetrics
  , newMetricsIO

  , extractValues
  , currentStats
  ) where

import Arbor.Monad.Counter.Type  (CounterId, CounterValue (CounterValue), CountersMap, Metrics (Metrics), MonadMetric)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.STM         (STM, atomically)
import Data.Generics.Product.Any

import qualified Arbor.Monad.Counter.Type as Z
import qualified Control.Concurrent.STM   as STM
import qualified Data.Map.Strict          as M

newMetricsIO :: IO Metrics
newMetricsIO = Metrics <$> STM.newTVarIO M.empty

newMetrics :: STM.STM Metrics
newMetrics = Metrics <$> STM.newTVar M.empty

-- Increase the current value by 1
incByKey :: MonadMetric m => CounterId -> m ()
incByKey = modifyByKey (+1)

-- Increase the current value by 1
incByKey' :: Metrics -> CounterId -> IO ()
incByKey' = modifyByKey' (+1)

-- Increase the current value by n
addByKey :: MonadMetric m => Int -> CounterId -> m ()
addByKey n = modifyByKey (+n)

-- Increase the current value by n
addByKey' :: Int -> Metrics -> CounterId -> IO ()
addByKey' n = modifyByKey' (+n)

-- Modify the current value with the supplied function
modifyByKey :: MonadMetric m => (Int -> Int) -> CounterId -> m ()
modifyByKey f key = do
  counters <- Z.getMetrics
  liftIO $ modifyByKey' f counters key

-- Modify the current value with the supplied function
modifyByKey' :: (Int -> Int) -> Metrics -> CounterId -> IO ()
modifyByKey' f (Metrics tCurrent) key = atomically $ do
  current <- STM.readTVar tCurrent
  case M.lookup key current of
    Just (CounterValue tv) -> STM.modifyTVar tv f
    Nothing -> do
      tv <- STM.newTVar (f 0)
      STM.writeTVar tCurrent (M.insert key (CounterValue tv) current)

-- Set the current value
setByKey :: MonadMetric m => Int -> CounterId -> m ()
setByKey value key = do
  counters <- Z.getMetrics
  liftIO $ setByKey' value counters key

-- Set the current value
setByKey' :: Int -> Metrics -> CounterId -> IO ()
setByKey' = modifyByKey' . const

-- valuesByKeys :: MonadMetric m => [CounterId] -> m [Int]
-- valuesByKeys ks = do
--   (Counters cur _) <- Z.getCounters
--   liftIO $ atomically $ sequence $ readTVar <$> ((\k -> cur M.! k ^. the @"var") <$> ks)

extractValues :: CountersMap -> STM ([(CounterId, Int)], [STM.TVar Int])
extractValues m = do
  let names = M.keys m
  let tvars = (^. the @"var") <$> M.elems m
  nums <- sequence $ STM.readTVar <$> tvars
  return (zip names nums, tvars)

currentStats :: MonadMetric m => m CountersMap
currentStats = do
  counters <- Z.getMetrics
  liftIO $ STM.readTVarIO $ counters ^. the @"counts"
