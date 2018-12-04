{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Arbor.Monad.MetricSpecApp
  ( runMetricSpecApp
  ) where

import Arbor.Monad.Metric
import Control.Monad.Catch
import Control.Monad.Logger      (LoggingT, MonadLogger, runLoggingT)
import Control.Monad.Reader
import Data.Generics.Product.Any
import GHC.Generics
import System.Log.FastLogger

newtype MiniConfig = MiniConfig
  { metrics      :: Metrics
  } deriving (Generic)

instance MonadMetrics MetricSpecApp where
  getMetrics = reader metrics

newtype MetricSpecApp a = MetricSpecApp
  { unMetricSpecApp :: ReaderT MiniConfig (LoggingT IO) a
  }
  deriving ( Functor
            , Applicative
            , Monad
            , MonadIO
            , MonadThrow
            , MonadCatch
            , MonadLogger
            , MonadReader MiniConfig)

runMetricSpecApp :: MetricSpecApp () -> IO ()
runMetricSpecApp f = do
  config <- MiniConfig <$> newMetricsIO
  runLoggingT (runReaderT (unMetricSpecApp f) config) $ \_ _ _ _ -> return ()
