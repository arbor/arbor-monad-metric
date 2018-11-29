{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Arbor.Monad.Metric.Type where

import Data.String
import GHC.Generics

import qualified Control.Concurrent.STM as STM

newtype MetricId = MetricId
  { name :: String
  } deriving (Eq, Ord, Show, IsString)

newtype Counter = Counter
  { id :: MetricId
  } deriving (Eq, Ord, Show)

newtype Gauge = Gauge
  { id :: MetricId
  } deriving (Eq, Ord, Show)

newtype CounterValue = CounterValue
  { var   :: STM.TVar Int
  } deriving (Generic)
