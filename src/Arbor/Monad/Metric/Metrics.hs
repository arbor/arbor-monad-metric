{-# LANGUAGE DeriveGeneric #-}

module Arbor.Monad.Metric.Metrics where

import Arbor.Monad.Metric.Type
import Data.Map.Strict
import GHC.Generics

import qualified Control.Concurrent.STM as STM

data Metrics = Metrics
  { counters :: STM.TVar (Map MetricId MetricValue)
  , gauges   :: STM.TVar (Map MetricId MetricValue)
  } deriving (Generic)
