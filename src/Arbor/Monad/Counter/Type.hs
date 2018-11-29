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

class (Monad m, MonadIO m) => MonadMetrics m where
  getMetrics :: m Metrics

instance MonadMetrics m => MonadMetrics (ExceptT e m) where
  getMetrics = lift getMetrics

instance MonadMetrics m => MonadMetrics (IdentityT m) where
  getMetrics = lift getMetrics

instance MonadMetrics m => MonadMetrics (MaybeT m) where
  getMetrics = lift getMetrics

instance MonadMetrics m => MonadMetrics (ReaderT e m) where
  getMetrics = lift getMetrics

instance MonadMetrics m => MonadMetrics (ResourceT m) where
  getMetrics = lift getMetrics

instance MonadMetrics m => MonadMetrics (StateT s m) where
  getMetrics = lift getMetrics
