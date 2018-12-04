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

import qualified Arbor.Network.StatsD      as S
import qualified Arbor.Network.StatsD.Type as Z

data MiniConfig = MiniConfig
  { metrics     :: Metrics
  , statsClient :: Z.StatsClient
  } deriving (Generic)

instance MonadMetrics MetricSpecApp where
  getMetrics = reader metrics

instance S.MonadStats MetricSpecApp where
  getStatsClient = reader statsClient

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
  let statsOpts = Z.DogStatsSettings "localhost" 6666
  statsClient <- S.createStatsClient statsOpts (Z.MetricName "MetricSpecApp") []
  metrics <- newMetricsIO
  let config = MiniConfig metrics statsClient
  runLoggingT (runReaderT (unMetricSpecApp f) config) $ \_ _ _ _ -> return ()
