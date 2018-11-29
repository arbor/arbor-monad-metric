{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Arbor.Monad.Counter.Type where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Data.Map.Strict
import GHC.Generics

import qualified Control.Concurrent.STM as STM

newtype CounterId = CounterId
  { name :: String
  } deriving (Eq, Ord, Show)

newtype CounterValue = CounterValue
  { var   :: STM.TVar Int
  } deriving (Generic)

type CountersMap = Map CounterId CounterValue

newtype Metrics = Metrics
  { counts  :: STM.TVar (Map CounterId CounterValue)
  } deriving (Generic)

class (Monad m, MonadIO m) => MonadMetric m where
  getMetrics :: m Metrics

instance MonadMetric m => MonadMetric (ExceptT e m) where
  getMetrics = lift getMetrics

instance MonadMetric m => MonadMetric (IdentityT m) where
  getMetrics = lift getMetrics

instance MonadMetric m => MonadMetric (MaybeT m) where
  getMetrics = lift getMetrics

instance MonadMetric m => MonadMetric (ReaderT e m) where
  getMetrics = lift getMetrics

instance MonadMetric m => MonadMetric (ResourceT m) where
  getMetrics = lift getMetrics

instance MonadMetric m => MonadMetric (StateT s m) where
  getMetrics = lift getMetrics
