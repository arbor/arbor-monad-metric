{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.Metric
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
  , currentCounts
  ) where

import Arbor.Monad.Metric.Type   (CountValue (CountValue), MetricId, Metrics (Metrics), MonadMetric)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.STM         (STM, atomically)
import Data.Generics.Product.Any

import qualified Arbor.Monad.Metric.Type as Z
import qualified Control.Concurrent.STM  as STM
import qualified Data.Map.Strict         as M

newMetricsIO :: IO Metrics
newMetricsIO = Metrics <$> STM.newTVarIO M.empty

newMetrics :: STM.STM Metrics
newMetrics = Metrics <$> STM.newTVar M.empty

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
  metrics <- Z.getMetrics
  liftIO $ modifyByKey' f metrics key

-- Modify the current value with the supplied function
modifyByKey' :: (Int -> Int) -> Metrics -> MetricId -> IO ()
modifyByKey' f (Metrics tCurrent) key = atomically $ do
  current <- STM.readTVar tCurrent
  case M.lookup key current of
    Just (CountValue tv) -> STM.modifyTVar tv f
    Nothing -> do
      tv <- STM.newTVar (f 0)
      STM.writeTVar tCurrent (M.insert key (CountValue tv) current)

-- Set the current value
setByKey :: MonadMetric m => Int -> MetricId -> m ()
setByKey value key = do
  metrics <- Z.getMetrics
  liftIO $ setByKey' value metrics key

-- Set the current value
setByKey' :: Int -> Metrics -> MetricId -> IO ()
setByKey' = modifyByKey' . const

extractValues :: M.Map MetricId CountValue -> STM ([(MetricId, Int)], [STM.TVar Int])
extractValues m = do
  let names = M.keys m
  let tvars = (^. the @"var") <$> M.elems m
  nums <- sequence $ STM.readTVar <$> tvars
  return (zip names nums, tvars)

currentCounts :: MonadMetric m => m (M.Map MetricId CountValue)
currentCounts = do
  metrics <- Z.getMetrics
  liftIO $ STM.readTVarIO $ metrics ^. the @"counts"
