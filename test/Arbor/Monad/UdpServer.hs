module Arbor.Monad.UdpServer
  ( createUdpServer
  , runUdpServer
  , UdpHandler
  ) where

import Network.Socket

import qualified Data.ByteString           as BS
import qualified Network.Socket.ByteString as BS

type UdpHandler = SockAddr -> BS.ByteString -> IO ()

createUdpServer :: ()
    => String       -- ^ Port number or name; 514 is default
    -> IO Socket
createUdpServer port = withSocketsDo $ do
  addrinfos <- getAddrInfo
              (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
              Nothing (Just port)
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  setSocketOption sock ReuseAddr 1
  setSocketOption sock ReusePort 1

  bind sock (addrAddress serveraddr)
  return sock

runUdpServer :: ()
  => Socket
  -> UdpHandler
  -> IO ()
runUdpServer sock handler = withSocketsDo $ procMessages sock
  where procMessages sock = do
          (msg, addr) <- BS.recvFrom sock 4096
          handler addr msg
          procMessages sock
