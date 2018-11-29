{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.Metric
  ( MonadMetric(..)

  , newMetrics
  , newMetricsIO

  , extractValues
  , currentCounters

  , adjust
  ) where

import Arbor.Monad.Metric.Class   (MonadMetric (..))
import Arbor.Monad.Metric.Metrics (Metrics (Metrics))
import Arbor.Monad.Metric.Type    (MetricId, MetricValue (MetricValue))
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

-- Modify the current value with the supplied function
adjust :: MonadMetric m
  => (Metrics -> STM.TVar (M.Map MetricId MetricValue))
  -> (Double -> Double)
  -> MetricId
  -> m ()
adjust field f key = do
  tMetrics <- getMetrics <&> field
  liftIO $ atomically $ do
    current <- STM.readTVar tMetrics
    case M.lookup key current of
      Just (MetricValue tv) -> STM.modifyTVar tv f
      Nothing -> do
        tv <- STM.newTVar (f 0)
        STM.writeTVar tMetrics (M.insert key (MetricValue tv) current)

extractValues :: M.Map MetricId MetricValue -> STM ([(MetricId, Double)], [STM.TVar Double])
extractValues m = do
  let names = M.keys m
  let tvars = (^. the @"var") <$> M.elems m
  nums <- sequence $ STM.readTVar <$> tvars
  return (zip names nums, tvars)

currentCounters :: MonadMetric m => m (M.Map MetricId MetricValue)
currentCounters = do
  metrics <- getMetrics
  liftIO $ STM.readTVarIO $ metrics ^. the @"counters"

data MetricType = CounterType | GaugeType
  deriving (Eq, Show)
