{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.MetricSpec
  ( spec
  ) where

import Control.Concurrent
import Control.Exception      (bracket)
import Control.Monad.IO.Class
import Data.Proxy

import qualified Arbor.Monad.Metric         as M
import qualified Arbor.Monad.Metric.Datadog as M
import qualified Arbor.Monad.Metric.Type    as M
import qualified Arbor.Monad.MetricSpecApp  as A
import qualified Arbor.Monad.UdpServer      as UDP
import qualified Control.Concurrent.STM     as STM
import qualified Data.ByteString.Char8      as BS
import qualified Data.Map.Strict            as MAP
import qualified Network.Socket             as S hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString  as S

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

handler :: STM.TVar [BS.ByteString] -> UDP.UdpHandler
handler tMsgs addr msg = do
  STM.atomically $ STM.modifyTVar tMsgs (msg:)
  putStrLn $ "From " ++ show addr ++ ": " ++ show msg

spec :: Spec
spec = describe "Arbor.Monad.MetricSpec" $ do
  it "Metrics library actually sends statsd messages over UDP" $ requireTest $ do
    tMessages <- liftIO $ STM.newTVarIO []
    sock <- liftIO $ UDP.createUdpServer "6666"
    threadId <- liftIO $ forkIO $ do
      UDP.runUdpServer sock (handler tMessages)
    liftIO $ threadDelay 3000000

    let gaugeExpected = "MetricSpecApp.gauge:20.000000|g|#stat:test.gauge\nMetricSpecApp.test.gauge:20.000000|g\n" :: BS.ByteString
    liftIO $ A.runMetricSpecApp $ do
      M.metric (M.Counter "test.counter") 10
      M.metric (M.Gauge "test.gauge") 20
      metrics <- M.getMetrics
      tCounterMap <- M.getMetricMapTVar
      (counters, _) <- liftIO . STM.atomically $ STM.swapTVar tCounterMap MAP.empty >>= M.extractValues (Proxy @M.Counter)
      liftIO $ putStrLn $ show counters
      tGaugeMap <- M.getMetricMapTVar
      (gauges, _) <- liftIO . STM.atomically $ STM.swapTVar tGaugeMap MAP.empty >>= M.extractValues (Proxy @M.Gauge)
      liftIO $ putStrLn $ show gauges
      M.logStats
      liftIO $ threadDelay 1000000

    liftIO $ threadDelay 6000000
    liftIO $ killThread threadId
    messages <- liftIO $ STM.readTVarIO tMessages
    messages === [gaugeExpected]
