{-# LANGUAGE LambdaCase #-}

module ZkFold.Bitcoin.CoinSelection (
  chooseCoinsMandatory,
) where

import Conduit (runIdentity)
import Data.Conduit (ConduitT, Void, await, runConduit, (.|))
import Data.Conduit.List (sourceList)
import Haskoin
import ZkFold.Bitcoin.Types.Internal.Common (Satoshi)

-- TODO: Coin selection does not support arbitrary scripts

-- | Like 'chooseCoins' but forces inclusion of mandatory coins.
chooseCoinsMandatory ::
  (Coin c) =>
  -- | mandatory coins
  [c] ->
  -- | value to send
  Satoshi ->
  -- | fee per byte
  Satoshi ->
  -- | number of outputs (including change)
  Int ->
  -- | try to find better solutions
  Bool ->
  -- | Other available coins to choose from
  [c] ->
  -- | coin selection and change
  Either String ([c], Satoshi)
chooseCoinsMandatory mandatory target fee nOut continue otherCoins =
  runIdentity . runConduit $
    sourceList otherCoins .| chooseCoinsSinkMandatory mandatory target fee nOut continue

chooseCoinsSinkMandatory ::
  (Monad m, Coin c) =>
  -- | Mandatory coins
  [c] ->
  -- | Value to send
  Satoshi ->
  -- | Fee per byte
  Satoshi ->
  -- | Number of outputs (including change)
  Int ->
  -- | Try to find better solution
  Bool ->
  ConduitT c Void m (Either String ([c], Satoshi))
chooseCoinsSinkMandatory mandatory target fee nOut continue =
  maybeToEither err
    <$> greedyAddSinkMandatory mandatory target (guessTxFee fee nOut) continue
 where
  err = "chooseCoinsSinkMandatory: No solution found"

{- | Like 'greedyAddSink' but forces inclusion of mandatory coins. Assumes the
other coins stream does not include the mandatory ones.
-}
greedyAddSinkMandatory ::
  (Monad m, Coin c) =>
  -- | Mandatory coins to include
  [c] ->
  -- | Value to send
  Satoshi ->
  -- | Coin count to fee function
  (Int -> Satoshi) ->
  -- | Try to find better solutions
  Bool ->
  ConduitT c Void m (Maybe ([c], Satoshi))
greedyAddSinkMandatory mandatory target guessFee continue = do
  if initial_tot >= goal initial_len
    then return $ Just (initial_acc, initial_tot - goal initial_len)
    else go initial_acc initial_tot [] 0
 where
  initial_acc = mandatory
  initial_tot = sum (coinValue <$> mandatory)
  initial_len = length mandatory
  goal c = target + guessFee c
  go acc aTot ps pTot =
    await >>= \case
      Just coin -> do
        let val = coinValue coin
            new_tot = val + aTot
            new_len = length acc + 1
            new_acc = coin : acc
        if new_tot >= goal new_len
          then
            if continue
              then
                if pTot == 0 || new_tot < pTot
                  then go initial_acc initial_tot new_acc new_tot
                  else return $ Just (ps, pTot - goal (length ps))
              else return $ Just (new_acc, new_tot - goal new_len)
          else go new_acc new_tot ps pTot
      Nothing ->
        return $
          if null ps
            then Nothing
            else Just (ps, pTot - goal (length ps))