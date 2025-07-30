# `zkfold-bitcoin-lib`

`zkfold-bitcoin-lib` is a library focused on interaction with Bitcoin blockchain, such as querying Bitcoin ledger, building transactions and submitting them. It extends [`haskoin-core`](https://github.com/jprupp/haskoin-core) for easy transaction building.

Library defines following monadic typeclasses, their usage is illustrated in `ZkFold.Bitcoin.Test.RegTest` module:

* `BitcoinQueryMonad`: Abstracts over ledger queries, such as UTxOs at an address and submitting a transaction. Use `runBitcoinQueryMonadIO` to interact with it.

  Example code:
  ```haskell
  -- Returns the block hash of genesis block.
  -- `bitcoinProvider` here is of type `BitcoinProvider`.
  runBitcoinQueryMonadIO bitcoinProvider $ blockHash 0
  ```
* `BitcoinBuilderMonad`: Allows building for transaction from `TxSkeleton`. Use `runBitcoinBuilderMonadIO` to interact with it.
* `BitcoinSignerMonad`: Allows signing of built transactions, though only for inputs locked under P2PKH, P2WPKH or a multi-sig (`PayMulSig` type in Haskoin). Use `runBitcoinSignerMonadIO` to interact with it. For complex scripts, see `ZkFold.Bitcoin.Test.RegTest` module for an end-to-end example.

We currently have support for raw bitcoin node (which can be ran in pruned mode, costing little of system's resources) and mempool.space.

Note that all methods provided under `BitcoinQueryMonad` are efficient for all the supported providers with following exceptions:
- `utxosAtAddress` is not efficient for locally ran bitcoin node.

## RegTest testing machinery

We support convenient transaction building to tests against private (regression) testnet. First we need to set it up by running `zkfold-bitcoin-lib/scripts/regtest.sh` after which we can interact with it as in `ZkFold.Bitcoin.Test.RegTest` module (`cabal run zkfold-bitcoin-lib-test`).

This test setup provides a wallet (identified by `testWalletAddress` in `ZkFold.Bitcoin.Test.Constants`) with 50 BTC initially and a block is minted every 10 seconds.