-- | Clients and servers on the loopback interface.
module Main (main) where

import Control.Concurrent.Async (mapConcurrently, withAsync)
import Control.Exception (bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS (concat, pack)
import Data.ByteString.Char8 qualified as BS8 (pack)
import GHC.Clock (getMonotonicTime)
import Network.Socket (ShutdownCmd (ShutdownSend), Socket, shutdown, socketPort)
import Network.Socket qualified as N (close)
import Streamly.Data.Fold (drainMapM)
import Streamly.Data.Stream (fold, toList)
import Streamly.Data.Stream qualified as S (take)
import Streamly.External.ByteString (fromArray, toArray)
import Streamly.External.Network.TCP (Transport (receive, send), connect,
                                      connectWithin, listen, opened,
                                      serveConnections, socketTransport)
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

-- | Serve with a function on a free port of the loopback interface, and
-- run something against the port.
withServer :: (Socket -> IO ()) -> (Int -> IO a) -> IO a
withServer client run =
  bracket (listen "127.0.0.1" 0) N.close $ \listener -> do
    port <- socketPort listener
    withAsync (serveConnections listener client) $ \_ -> run (fromIntegral port)

-- | Send back what comes.
echo :: Socket -> IO ()
echo peer = let transport = socketTransport peer in fold (drainMapM transport.send) transport.receive

-- | Send bytes, say that nothing more comes, and read everything back.
exchange :: Int -> ByteString -> IO ByteString
exchange port bytes = bracket (connect "localhost" port) N.close $ \socket -> do
  let transport = socketTransport socket
  transport.send (toArray bytes)
  shutdown socket ShutdownSend
  BS.concat . map fromArray <$> toList transport.receive

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000) $
    testGroup
      "streamly-network-tcp"
      [ testCase "bytes go to a server and come back, megabytes of them" $ do
          let payload = BS.pack (take 3_000_000 (cycle [0 .. 250]))
          echoed <- withServer echo (\port -> exchange port payload)
          echoed @?= payload
      , testCase "each client is served in a thread of its own" $ do
          answers <- withServer echo $ \port ->
            mapConcurrently (\index -> exchange port (BS8.pack (show index))) [1 .. 30 :: Int]
          answers @?= map (BS8.pack . show) [1 .. 30 :: Int]
      , testCase "a client whose function fails does not stop the server" $ do
          let fragile peer = do
                let transport = socketTransport peer
                first <- toList (S.take 1 transport.receive)
                if map fromArray first == ["fail"] then ioError (userError "on purpose") else transport.send (toArray "served")
          answers <- withServer fragile (\port -> mapM (exchange port) ["fail", "work", "fail", "work"])
          answers @?= ["", "served", "", "served"]
      , testCase "the receiving of a transport ends when the other side is done" $ do
          answer <- withServer (\peer -> (socketTransport peer).send (toArray "bye")) (\port -> exchange port "")
          answer @?= "bye"
      , testCase "an opened socket gives its transport and what closes it" $ do
          answer <- withServer echo $ \port -> do
            (transport, close) <- connect "127.0.0.1" port >>= opened
            transport.send (toArray "ping")
            first <- toList (S.take 1 transport.receive)
            close
            pure (BS.concat (map fromArray first))
          answer @?= "ping"
      , testCase "nothing listening is an IOError, with a time limit or without" $ do
          port <- bracket (listen "127.0.0.1" 0) N.close (fmap fromIntegral . socketPort)
          try (connect "127.0.0.1" port) >>= \case
            Left (_ :: IOError) -> pure ()
            Right socket -> N.close socket >> assertFailure "connected to nothing"
          try (connectWithin (Just 2_000_000) "127.0.0.1" port) >>= \case
            Left (_ :: IOError) -> pure ()
            Right _ -> assertFailure "connected to nothing"
      , testCase "connecting gives up in time" $ do
          -- An address that answers nothing where it is routed, and is
          -- refused at once where it is not: either way the wait is
          -- bounded.
          before <- getMonotonicTime
          outcome <- try (connectWithin (Just 200_000) "10.255.255.1" 81)
          after <- getMonotonicTime
          case outcome of
            Left (_ :: IOError) -> pure ()
            Right Nothing -> pure ()
            Right (Just socket) -> N.close socket
          assertBool ("waited " <> show (after - before) <> " s") (after - before < 2)
      , testCase "connecting without a limit reaches a server" $ do
          answer <- withServer echo $ \port ->
            connectWithin Nothing "127.0.0.1" port >>= \case
              Nothing -> assertFailure "no connection" >> pure ""
              Just socket -> bracket (pure socket) N.close $ \_ -> do
                (socketTransport socket).send (toArray "x")
                BS.concat . map fromArray <$> toList (S.take 1 (socketTransport socket).receive)
          answer @?= "x"
      ]
