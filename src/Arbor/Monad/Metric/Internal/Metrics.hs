{-# LANGUAGE DeriveGeneric #-}

module Arbor.Monad.Metric.Internal.Metrics where

import Arbor.Monad.Metric.Type
import Data.Map.Strict
import GHC.Generics

import qualified Control.Concurrent.STM as STM

newtype Metrics = Metrics
  { counters  :: STM.TVar (Map MetricId CounterValue)
  } deriving (Generic)
