{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.Metric
  ( MonadMetric(..)

  , incByKey
  , incByKey'
  , addByKey
  , addByKey'
  , setByKey
  , setByKey'

  , newMetrics
  , newMetricsIO

  , extractValues
  , currentCounters
  ) where

import Arbor.Monad.Metric.Class   (MonadMetric (..))
import Arbor.Monad.Metric.Metrics (Metrics (Metrics))
import Arbor.Monad.Metric.Type    (CounterValue (CounterValue), MetricId)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.STM          (STM, atomically)
import Data.Generics.Product.Any

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict        as M

newMetricsIO :: IO Metrics
newMetricsIO = Metrics
  <$> STM.newTVarIO M.empty
  <*> STM.newTVarIO M.empty

newMetrics :: STM.STM Metrics
newMetrics = Metrics
  <$> STM.newTVar M.empty
  <*> STM.newTVar M.empty

-- Increase the current value by 1
incByKey :: MonadMetric m => MetricId -> m ()
incByKey = modifyByKey (+1)

-- Increase the current value by 1
incByKey' :: Metrics -> MetricId -> IO ()
incByKey' = modifyByKey' (+1)

-- Increase the current value by n
addByKey :: MonadMetric m => Int -> MetricId -> m ()
addByKey n = modifyByKey (+n)

-- Increase the current value by n
addByKey' :: Int -> Metrics -> MetricId -> IO ()
addByKey' n = modifyByKey' (+n)

-- Modify the current value with the supplied function
modifyByKey :: MonadMetric m => (Int -> Int) -> MetricId -> m ()
modifyByKey f key = do
  metrics <- getMetrics
  liftIO $ modifyByKey' f metrics key

-- Modify the current value with the supplied function
modifyByKey' :: (Int -> Int) -> Metrics -> MetricId -> IO ()
modifyByKey' f (Metrics tCurrent _) key = atomically $ do
  current <- STM.readTVar tCurrent
  case M.lookup key current of
    Just (CounterValue tv) -> STM.modifyTVar tv f
    Nothing -> do
      tv <- STM.newTVar (f 0)
      STM.writeTVar tCurrent (M.insert key (CounterValue tv) current)

-- Set the current value
setByKey :: MonadMetric m => Int -> MetricId -> m ()
setByKey value key = do
  metrics <- getMetrics
  liftIO $ setByKey' value metrics key

-- Set the current value
setByKey' :: Int -> Metrics -> MetricId -> IO ()
setByKey' = modifyByKey' . const

extractValues :: M.Map MetricId CounterValue -> STM ([(MetricId, Int)], [STM.TVar Int])
extractValues m = do
  let names = M.keys m
  let tvars = (^. the @"var") <$> M.elems m
  nums <- sequence $ STM.readTVar <$> tvars
  return (zip names nums, tvars)

currentCounters :: MonadMetric m => m (M.Map MetricId CounterValue)
currentCounters = do
  metrics <- getMetrics
  liftIO $ STM.readTVarIO $ metrics ^. the @"counters"
