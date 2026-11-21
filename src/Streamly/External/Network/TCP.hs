-- |
-- Module      : Streamly.External.Network.TCP
-- Description : TCP connections, for clients and servers, and transports
-- License     : BSD-3-Clause
--
-- What the network libraries of this family share: opening a TCP
-- connection to a host by its name, listening and serving, and the
-- 'Transport' that protocols are written against.
--
-- > {-# LANGUAGE OverloadedRecordDot #-}
-- > import Control.Exception (bracket)
-- > import Network.Socket (close)
-- > import Streamly.Data.Fold qualified as Fold
-- > import Streamly.Data.Stream qualified as Stream
-- > import Streamly.External.Network.TCP
-- >
-- > -- Send back to each client what it sends.
-- > main :: IO ()
-- > main = bracket (listen "127.0.0.1" 7000) close $ \listener ->
-- >   serveConnections listener $ \peer ->
-- >     let transport = socketTransport peer
-- >      in Stream.fold (Fold.drainMapM transport.send) transport.receive
module Streamly.External.Network.TCP
  ( -- * Transports
    Transport (..)
  , socketTransport
  , opened

    -- * Clients
  , connect
  , connectWithin

    -- * Servers
  , listen
  , serveConnections
  ) where

import Control.Concurrent (forkIOWithUnmask)
import Control.Exception (IOException, SomeException, bracketOnError, finally, mask_, throwIO, try)
import Control.Monad (forever, void)
import Data.Word (Word8)
import Network.Socket qualified as Net
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Network.Socket qualified as Socket
import System.Timeout (timeout)

-- | A connection that is open, as the two things a protocol needs from
-- it. A protocol written against a transport runs over plain TCP, over
-- TLS, or over anything else that gives these two.
data Transport = Transport
  { send :: Array Word8 -> IO ()
  -- ^ Send bytes.
  , receive :: Stream IO (Array Word8)
  -- ^ The bytes received, until the other side ends the connection.
  }

-- | A TCP socket as a transport.
socketTransport :: Net.Socket -> Transport
socketTransport socket = Transport {send = Socket.putChunk socket, receive = Socket.readChunks socket}

-- | A connected socket as a transport and what closes it, for the
-- connections that outlive a function. Small writes are not held back
-- (@TCP_NODELAY@): on a connection that carries several exchanges, a
-- request that waits for the last answer to be acknowledged costs tens of
-- milliseconds each time.
opened :: Net.Socket -> IO (Transport, IO ())
opened socket = do
  Net.setSocketOption socket Net.NoDelay 1
  pure (socketTransport socket, Net.close socket)

-- | Open a TCP connection to a host and a port: to the first address of
-- the host that accepts it. Throws 'IOError', that of the last address
-- tried.
connect :: String -> Int -> IO Net.Socket
connect host port = do
  addresses <- Net.getAddrInfo (Just hints) (Just host) (Just (show port))
  attempt addresses
  where
    hints = Net.defaultHints {Net.addrSocketType = Net.Stream}
    attempt [] = throwIO (userError ("no address for " <> host))
    attempt (address : others) = do
      outcome <- try (to address)
      case (outcome, others) of
        (Right socket, _) -> pure socket
        (Left (problem :: IOException), []) -> throwIO problem
        (Left _, _) -> attempt others
    to address =
      bracketOnError (Net.openSocket address) Net.close $ \socket -> do
        Net.connect socket (Net.addrAddress address)
        pure socket

-- | Like 'connect', within a number of microseconds for the whole of it,
-- name resolution included; 'Nothing' as the limit waits without end.
-- Gives 'Nothing' when the time is exceeded: what was opened is closed.
-- Call it unmasked, or the limit cannot stop it.
connectWithin :: Maybe Int -> String -> Int -> IO (Maybe Net.Socket)
connectWithin limit host port = case limit of
  Nothing -> Just <$> connect host port
  Just microseconds -> timeout microseconds (connect host port)

-- | A TCP socket that listens on an address and a port; 0 lets the system
-- choose the port, which 'Net.socketPort' then tells.
listen :: String -> Int -> IO Net.Socket
listen host port = do
  addresses <- Net.getAddrInfo (Just hints) (Just host) (Just (show port))
  case addresses of
    [] -> throwIO (userError ("no address for " <> host))
    address : _ ->
      bracketOnError (Net.openSocket address) Net.close $ \listener -> do
        Net.setSocketOption listener Net.ReuseAddr 1
        Net.bind listener (Net.addrAddress address)
        Net.listen listener 128
        pure listener
  where
    hints = Net.defaultHints {Net.addrSocketType = Net.Stream, Net.addrFlags = [Net.AI_PASSIVE]}

-- | Serve every client that connects to a listening socket with a
-- function, each in a thread of its own, without end. The connection of a
-- client is closed when the function returns or throws; what it throws
-- ends that client only and is not reported.
serveConnections :: Net.Socket -> (Net.Socket -> IO ()) -> IO ()
serveConnections listener client = forever . mask_ $ do
  (peer, _) <- Net.accept listener
  void (forkIOWithUnmask (\unmask -> void (try @SomeException (unmask (client peer))) `finally` Net.close peer))
