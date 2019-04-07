{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Arbor.Monad.MetricSpec
  ( spec
  ) where

import Control.Concurrent
import Control.Exception         (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics.Product.Any
import Data.Proxy
import Data.Semigroup

import qualified Arbor.Monad.Metric        as M
import qualified Arbor.Monad.Metric.Type   as MT
import qualified Arbor.Monad.MetricApp     as A
import qualified Control.Concurrent.STM    as STM
import qualified Data.ByteString.Char8     as BS
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import qualified Network.Socket            as S hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString as S

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "Arbor.Monad.MetricSpec" $ do
  it "Metrics library actually tracks metrics it receives" $ requireTest $ do
    metrics <- liftIO $ A.runMetricApp $ do
      M.metric (M.counter "test.counter"                                        ) 10
      M.metric (M.gauge   "test.gauge"                                          ) 20
      M.metric (M.gauge   "test.gauge"  & the @"tags" .~ M.tags [("foo", "bar")]) 30
      MT.getMetrics

    countersMap <- liftIO $ STM.readTVarIO $ metrics ^. the @"counters"
    gaugesMap   <- liftIO $ STM.readTVarIO $ metrics ^. the @"gauges"

    counters  <- liftIO $ STM.atomically $ fst <$> M.extractValues @MT.Counter Proxy countersMap
    gauges    <- liftIO $ STM.atomically $ fst <$> M.extractValues @MT.Gauge   Proxy gaugesMap

    counters ===  [ (M.counter "test.counter"                                         , 10)
                  ]

    gauges   ===  [ (M.gauge   "test.gauge"                                           , 20)
                  , (M.gauge   "test.gauge"  & the @"tags" .~ M.tags [("foo", "bar")] , 30)
                  ]
