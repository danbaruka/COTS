{-# LANGUAGE OverloadedStrings #-}

-- | UTXO management for simulation
module COTS.Simulation.UTXO
  ( validateUTXOs
  , spendUTXOs
  , createUTXOs
  , calculateUTXOSize
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Data.Map.Strict as Map

import COTS.Types

-- | Validate UTXOs for a wallet
validateUTXOs :: Text -> [UTXO] -> Either SimulationError ()
validateUTXOs walletName utxos = do
  -- Check if wallet has any UTXOs
  when (null utxos) $
    Left (InsufficientFunds $ "Wallet " <> walletName <> " has no UTXOs")
  
  -- Check if UTXOs have sufficient funds
  let totalLovelace = sum $ map (lovelace . amount) utxos
  when (totalLovelace == 0) $
    Left (InsufficientFunds $ "Wallet " <> walletName <> " has no lovelace")
  
  return ()

-- | Spend UTXOs and return remaining change
spendUTXOs :: [UTXO] -> Word64 -> Either SimulationError ([UTXO], [UTXO])
spendUTXOs utxos requiredAmount = do
  let totalAvailable = sum $ map (lovelace . amount) utxos
  
  when (totalAvailable < requiredAmount) $
    Left (InsufficientFunds $ "Insufficient funds. Required: " <> T.pack (show requiredAmount) <> ", Available: " <> T.pack (show totalAvailable))
  
  -- Simple spending strategy: spend UTXOs in order until we have enough
  let (spent, remaining) = spendUTXOsHelper utxos requiredAmount []
  
  return (spent, remaining)

-- | Helper function to spend UTXOs
spendUTXOsHelper :: [UTXO] -> Word64 -> [UTXO] -> ([UTXO], [UTXO])
spendUTXOsHelper [] _ spent = (spent, [])
spendUTXOsHelper (utxo:rest) requiredAmount spent =
  let currentSpent = sum $ map (lovelace . amount) spent
      utxoAmount = lovelace (amount utxo)
  in if currentSpent >= requiredAmount
     then (spent, utxo:rest)
     else spendUTXOsHelper rest requiredAmount (utxo:spent)

-- | Create new UTXOs from transaction outputs
createUTXOs :: [TransactionOutput] -> TransactionId -> [UTXO]
createUTXOs outputs txId = 
  zipWith (\output index -> UTXO
    { txHash = txId
    , txIx = TxIndex index
    , amount = outputAmount output
    }) outputs [0..]

-- | Calculate UTXO size for fee calculation
calculateUTXOSize :: UTXO -> Word64
calculateUTXOSize utxo = 
  let baseSize = 64 -- Base size for UTXO
      assetCount = fromIntegral $ Map.size (assets (amount utxo))
      assetSize = assetCount * 32 -- Approximate size per asset
  in baseSize + assetSize 