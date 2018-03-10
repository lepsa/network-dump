module Main where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Foldable
import           Data.Traversable
import           IPs
import           Network.Simple.TCP

main :: IO ()
main = concurrently_
  (connect "localhost" "30001" (streamHosts "30002")) -- Raw messages
  (connect "localhost" "30005" (streamHosts "30004")) -- Beast messages

streamHosts :: ServiceName -> (Socket, SockAddr) -> IO ()
streamHosts p (s, _) = runConcurrently $ traverse_ (streamConcurrent s p) ips

streamConcurrent :: Socket -> ServiceName -> HostName -> Concurrently ()
streamConcurrent s p h = Concurrently $ catch (stream s p h) err
  where
    err :: SomeException -> IO ()
    err = const $ pure ()

stream :: Socket -> ServiceName -> HostName -> IO ()
stream s p h = connect h p $ \(s', _) ->
  forever $ recv s' 10000 >>= maybe (pure ()) (send s)
