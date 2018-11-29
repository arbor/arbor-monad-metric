module Arbor.Monad.Metric.Class where

import Arbor.Monad.Metric.Metrics
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource

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
