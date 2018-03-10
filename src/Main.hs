module Main where

import           Control.Applicative
import           Control.Concurrent
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
streamHosts p (s, _) = runConcurrently $ traverse_ (Concurrently . stream s p) ips

stream :: Socket -> ServiceName -> HostName -> IO ()
stream s p h = catch (connect h p f) err
  where
    f (s', _) = forever $ recv s' 10000 >>= maybe (pure ()) (send s)
    err :: SomeException -> IO ()
    err = const $ threadDelay (60 * 1000 * 1000) >> stream s p h
