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

type CounterKey = String

newtype CounterValue = CounterValue
  { var   :: STM.TVar Int
  } deriving (Generic)

type CountersMap = Map CounterKey CounterValue

newtype Counters = Counters
  { current  :: STM.TVar (Map CounterKey CounterValue)
  } deriving (Generic)

class (Monad m, MonadIO m) => MonadCounters m where
  getCounters :: m Counters

instance MonadCounters m => MonadCounters (ExceptT e m) where
  getCounters = lift getCounters

instance MonadCounters m => MonadCounters (IdentityT m) where
  getCounters = lift getCounters

instance MonadCounters m => MonadCounters (MaybeT m) where
  getCounters = lift getCounters

instance MonadCounters m => MonadCounters (ReaderT e m) where
  getCounters = lift getCounters

instance MonadCounters m => MonadCounters (ResourceT m) where
  getCounters = lift getCounters

instance MonadCounters m => MonadCounters (StateT s m) where
  getCounters = lift getCounters
