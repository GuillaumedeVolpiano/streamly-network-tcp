-- | Clients and servers on the loopback interface.
module Main (main) where

import Control.Concurrent.Async (mapConcurrently, withAsync)
import Control.Exception (bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import GHC.Clock (getMonotonicTime)
import Network.Socket qualified as Net
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.Network.TCP
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

-- | Serve with a function on a free port of the loopback interface, and
-- run something against the port.
withServer :: (Net.Socket -> IO ()) -> (Int -> IO a) -> IO a
withServer client run =
  bracket (listen "127.0.0.1" 0) Net.close $ \listener -> do
    port <- Net.socketPort listener
    withAsync (serveConnections listener client) $ \_ -> run (fromIntegral port)

-- | Send back what comes.
echo :: Net.Socket -> IO ()
echo peer = let transport = socketTransport peer in Stream.fold (Fold.drainMapM transport.send) transport.receive

-- | Send bytes, say that nothing more comes, and read everything back.
exchange :: Int -> ByteString -> IO ByteString
exchange port bytes = bracket (connect "localhost" port) Net.close $ \socket -> do
  let transport = socketTransport socket
  transport.send (StreamlyBS.toArray bytes)
  Net.shutdown socket Net.ShutdownSend
  BS.concat . map StreamlyBS.fromArray <$> Stream.toList transport.receive

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
                first <- Stream.toList (Stream.take 1 transport.receive)
                if map StreamlyBS.fromArray first == ["fail"] then ioError (userError "on purpose") else transport.send (StreamlyBS.toArray "served")
          answers <- withServer fragile (\port -> mapM (exchange port) ["fail", "work", "fail", "work"])
          answers @?= ["", "served", "", "served"]
      , testCase "the receiving of a transport ends when the other side is done" $ do
          answer <- withServer (\peer -> (socketTransport peer).send (StreamlyBS.toArray "bye")) (\port -> exchange port "")
          answer @?= "bye"
      , testCase "an opened socket gives its transport and what closes it" $ do
          answer <- withServer echo $ \port -> do
            (transport, close) <- connect "127.0.0.1" port >>= opened
            transport.send (StreamlyBS.toArray "ping")
            first <- Stream.toList (Stream.take 1 transport.receive)
            close
            pure (BS.concat (map StreamlyBS.fromArray first))
          answer @?= "ping"
      , testCase "nothing listening is an IOError, with a time limit or without" $ do
          port <- bracket (listen "127.0.0.1" 0) Net.close (fmap fromIntegral . Net.socketPort)
          try (connect "127.0.0.1" port) >>= \case
            Left (_ :: IOError) -> pure ()
            Right socket -> Net.close socket >> assertFailure "connected to nothing"
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
            Right (Just socket) -> Net.close socket
          assertBool ("waited " <> show (after - before) <> " s") (after - before < 2)
      , testCase "connecting without a limit reaches a server" $ do
          answer <- withServer echo $ \port ->
            connectWithin Nothing "127.0.0.1" port >>= \case
              Nothing -> assertFailure "no connection" >> pure ""
              Just socket -> bracket (pure socket) Net.close $ \_ -> do
                (socketTransport socket).send (StreamlyBS.toArray "x")
                BS.concat . map StreamlyBS.fromArray <$> Stream.toList (Stream.take 1 (socketTransport socket).receive)
          answer @?= "x"
      ]
