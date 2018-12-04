module Arbor.Monad.MetricSpec
  ( spec
  ) where

import Control.Concurrent
import Control.Exception      (bracket)
import Control.Monad.IO.Class

import qualified Arbor.Monad.UdpServer     as UDP
import qualified Control.Concurrent.STM    as STM
import qualified Data.ByteString.Char8     as BS
import qualified Network.Socket            as S hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString as S

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
    sock <- liftIO $ UDP.createUdpServer "5555"
    threadId <- liftIO $ forkIO $ do
      UDP.runUdpServer sock (handler tMessages)
    liftIO $ threadDelay 1000000
    liftIO $ killThread threadId
    messages <- liftIO $ STM.readTVarIO tMessages
    messages === []
