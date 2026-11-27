# Revision history for streamly-network-tcp

## 0.1.0.2 -- 2026-11-27

* No change of interface. The dependency bounds are stated once in the
  cabal file, and the imports list the names they use.
* The examples of the documentation import the names they use; each was
  typechecked.

## 0.1.0.1 -- 2026-11-27

* Builds with GHC 9.8 and 9.10 as well as 9.12: the language is GHC2021
  with the extensions GHC2024 adds, and the bounds on `base` and on the
  libraries that come with the compiler are wider.
* The links of the documentation say which name they mean, where a type
  and its constructor, or two modules, share one.

## 0.1.0.0 -- 2026-11-21

* `Transport`, `socketTransport`, `opened`.
* `connect`, `connectWithin`.
* `listen`, `serveConnections`.
