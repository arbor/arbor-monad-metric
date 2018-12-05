{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Arbor.Monad.MetricApp
  ( runMetricApp
  ) where

import Arbor.Monad.Metric
import Control.Monad.Reader
import Data.Generics.Product.Any
import GHC.Generics

import qualified Arbor.Network.StatsD      as S
import qualified Arbor.Network.StatsD.Type as Z

newtype MiniConfig = MiniConfig
  { metrics     :: Metrics
  } deriving (Generic)

instance MonadMetrics MetricApp where
  getMetrics = reader metrics

newtype MetricApp a = MetricApp
  { unMetricApp :: ReaderT MiniConfig IO a
  }
  deriving  ( Functor
            , Applicative
            , Monad
            , MonadIO
            , MonadReader MiniConfig)

runMetricApp :: MetricApp a -> IO a
runMetricApp f = do
  metrics <- newMetricsIO
  let config = MiniConfig metrics
  runReaderT (unMetricApp f) config
