# streamly-network-tcp

TCP connections for [Streamly](https://streamly.composewell.com/), for
clients and for servers, over the
[network](https://hackage.haskell.org/package/network) package. It is the
ground under the network libraries of this family
([streamly-http](https://github.com/GuillaumedeVolpiano/streamly-http),
[streamly-websockets](https://github.com/GuillaumedeVolpiano/streamly-websockets),
[streamly-network-tls](https://github.com/GuillaumedeVolpiano/streamly-network-tls)).

```haskell
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Exception (bracket)
import Network.Socket (close)
import Streamly.Data.Fold (drainMapM)
import Streamly.Data.Stream (fold)
import Streamly.External.Network.TCP (Transport (..), listen, serveConnections,
                                      socketTransport)

-- Send back to each client what it sends.
main :: IO ()
main = bracket (listen "127.0.0.1" 7000) close $ \listener ->
  serveConnections listener $ \peer ->
    let transport = socketTransport peer
     in fold (drainMapM transport.send) transport.receive
```

## What it gives

- `Transport`: a way to send byte arrays and the stream of those received.
  Protocols written against it run over plain TCP (`socketTransport`),
  over TLS, or over anything else that gives the two.
- `connect` and `connectWithin`: a connection to a host by its name, to
  the first of its addresses that accepts it, within a time if asked.
- `listen` and `serveConnections`: a listening socket, and each client
  served in a thread of its own.
- `opened`: a connected socket as a transport and what closes it, for the
  connections that outlive a function.
