{-# LANGUAGE OverloadedStrings #-}

-- | Ouroboros consensus rules
module COTS.Protocol.Consensus
  ( validateTransaction
  , validateUTXOSet
  , validateScriptExecution
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

import COTS.Types

-- | Validate transaction according to consensus rules
validateTransaction :: ProtocolParameters -> Transaction -> Either SimulationError ()
validateTransaction params transaction = do
  -- Validate transaction size
  validateTransactionSize params transaction
  
  -- Validate UTXO inputs
  validateTransactionInputs transaction
  
  -- Validate transaction outputs
  validateTransactionOutputs params transaction
  
  -- Validate script execution
  validateTransactionScripts params transaction

-- | Validate transaction size
validateTransactionSize :: ProtocolParameters -> Transaction -> Either SimulationError ()
validateTransactionSize params transaction = do
  let estimatedSize = estimateTransactionSize (length (txInputs transaction)) (length (txOutputs transaction))
  
  when (estimatedSize > maxTxSize params) $
    Left (ValidationFailed $ "Transaction size exceeds limit: " <> T.pack (show estimatedSize) <> " > " <> T.pack (show (maxTxSize params)))

-- | Validate transaction inputs
validateTransactionInputs :: Transaction -> Either SimulationError ()
validateTransactionInputs transaction = do
  -- Check for duplicate inputs
  let inputIds = map (\input -> (inputTxId input, inputTxIx input)) (txInputs transaction)
      uniqueInputs = length (txInputs transaction)
      uniqueInputIds = length (nub inputIds)
  
  when (uniqueInputs /= uniqueInputIds) $
    Left (ValidationFailed "Duplicate transaction inputs")

-- | Validate transaction outputs
validateTransactionOutputs :: ProtocolParameters -> Transaction -> Either SimulationError ()
validateTransactionOutputs params transaction = do
  mapM_ (validateTransactionOutput params) (txOutputs transaction)

-- | Validate individual transaction output
validateTransactionOutput :: ProtocolParameters -> TransactionOutput -> Either SimulationError ()
validateTransactionOutput params output = do
  -- Validate output amount
  when (lovelace (outputAmount output) == 0) $
    Left (ValidationFailed "Transaction output cannot have zero lovelace")
  
  -- Validate output size
  let outputSize = estimateOutputSize output
  when (outputSize > maxValSize params) $
    Left (ValidationFailed $ "Output size exceeds limit: " <> T.pack (show outputSize) <> " > " <> T.pack (show (maxValSize params)))

-- | Validate transaction scripts
validateTransactionScripts :: ProtocolParameters -> Transaction -> Either SimulationError ()
validateTransactionScripts params transaction = do
  mapM_ (validateScript params) (txScripts transaction)

-- | Validate individual script
validateScript :: ProtocolParameters -> PlutusScript -> Either SimulationError ()
validateScript params script = do
  -- Validate script size
  let scriptSize = T.length (scriptBytes script)
  when (fromIntegral scriptSize > maxValSize params) $
    Left (ValidationFailed $ "Script size exceeds limit: " <> T.pack (show scriptSize) <> " > " <> T.pack (show (maxValSize params)))

-- | Validate UTXO set
validateUTXOSet :: [UTXO] -> Either SimulationError ()
validateUTXOSet utxos = do
  -- Check for duplicate UTXOs
  let utxoIds = map (\utxo -> (txHash utxo, txIx utxo)) utxos
      uniqueUtxos = length utxos
      uniqueUtxoIds = length (nub utxoIds)
  
  when (uniqueUtxos /= uniqueUtxoIds) $
    Left (ValidationFailed "Duplicate UTXOs in set")

-- | Validate script execution
validateScriptExecution :: ProtocolParameters -> ExecutionUnits -> Either SimulationError ()
validateScriptExecution params units = do
  let maxUnits = maxExecutionUnitsPerTransaction params
  
  when (memory units > memory maxUnits) $
    Left (ScriptExecutionFailed $ "Memory limit exceeded: " <> T.pack (show (memory units)) <> " > " <> T.pack (show (memory maxUnits)))
  
  when (steps units > steps maxUnits) $
    Left (ScriptExecutionFailed $ "Steps limit exceeded: " <> T.pack (show (steps units)) <> " > " <> T.pack (show (steps maxUnits)))

-- | Estimate transaction size
estimateTransactionSize :: Int -> Int -> Word64
estimateTransactionSize inputCount outputCount = 
  let inputSize = fromIntegral inputCount * 64
      outputSize = fromIntegral outputCount * 128
      headerSize = 256
  in headerSize + inputSize + outputSize

-- | Estimate output size
estimateOutputSize :: TransactionOutput -> Word64
estimateOutputSize output = 
  let addressSize = 64
      amountSize = 64
      datumSize = maybe 0 (T.length . datumBytes) (outputDatum output)
  in addressSize + amountSize + fromIntegral datumSize

-- | Remove duplicates from list
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs) 