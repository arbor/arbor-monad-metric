{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}

module Arbor.Monad.MetricSpecApp
  ( runMetricSpecApp
  ) where

import Control.Monad.Catch
import Control.Monad.Logger      (LoggingT, MonadLogger, runLoggingT)
import Control.Monad.Reader
import Data.Generics.Product.Any
import GHC.Generics
import System.Log.FastLogger

newtype MiniConfig = MiniConfig
  { env      :: Int
  } deriving (Generic)

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
  let config = MiniConfig 0
  runLoggingT (runReaderT (unMetricSpecApp f) config) $ \_ _ _ _ -> return ()
