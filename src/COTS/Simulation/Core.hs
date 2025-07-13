{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Core simulation logic
module COTS.Simulation.Core
  ( simulateTransaction
  , SimulationContext(..)
  , SimulationState(..)
  , SimulationResult(..)
  , runSimulation
  , selectOptimalUTXOs
  , validateTransaction
  ) where

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import Data.List (sortBy, find)
import Data.Ord (comparing)

import COTS.Simulation.UTXO
import COTS.Simulation.Fees
import COTS.Simulation.Plutus
import COTS.Types
import qualified COTS.Types as Types

-- | Simulation context
data SimulationContext = SimulationContext
  { config :: Config
  , fromWallet :: Maybe Text
  , toAddress :: Maybe Address
  , simAmount :: Maybe Word64
  , script :: Maybe PlutusScript
  , datum :: Maybe Datum
  , redeemer :: Maybe Redeemer
  }

-- | Simulation state
data SimulationState = SimulationState
  { currentUTXOs :: Map.Map Text [UTXO] -- wallet name -> UTXOs
  , simTransaction :: Maybe Transaction
  , simErrors :: [SimulationError]
  , simExecutionUnits :: Maybe ExecutionUnits
  , selectedUTXOs :: [UTXO] -- UTXOs selected for spending
  , changeAmount :: Word64 -- Change amount
  }



-- | Simulate a transaction
simulateTransaction :: SimulationContext -> SimulationResult
simulateTransaction ctx = 
  let initialState = SimulationState
        { currentUTXOs = Map.fromList [(name wallet, utxos wallet) | wallet <- wallets (config ctx)]
        , simTransaction = Nothing
        , simErrors = []
        , simExecutionUnits = Nothing
        , selectedUTXOs = []
        , changeAmount = 0
        }
      finalState = execState (runSimulation ctx) initialState
  in SimulationResult
       { success = null (simErrors finalState)
       , transaction = simTransaction finalState
       , feeCalculation = calculateFeesForTransaction ctx finalState
       , errors = simErrors finalState
       , finalUTXOs = currentUTXOs finalState
       , executionUnits = simExecutionUnits finalState
       , simulationDetails = createSimulationDetails finalState
       }

-- | Run simulation in State monad
runSimulation :: SimulationContext -> State SimulationState ()
runSimulation ctx = do
  -- Validate inputs
  validateInputs ctx
  
  -- Check if we have any errors so far
  currentErrors <- gets simErrors
  when (null currentErrors) $ do
    -- Select optimal UTXOs
    selectUTXOsForTransaction ctx
    
    -- Validate transaction feasibility
    validateTransactionFeasibility ctx
    
    -- Check if we have any errors after UTXO selection
    currentErrors2 <- gets simErrors
    when (null currentErrors2) $ do
      -- Process transaction
      processTransaction ctx
      
      -- Update UTXOs
      updateUTXOs ctx

-- | Validate simulation inputs
validateInputs :: SimulationContext -> State SimulationState ()
validateInputs ctx = do
  -- Validate wallet exists
  case fromWallet ctx of
    Just walletName -> do
      wallets <- gets currentUTXOs
      when (not $ Map.member walletName wallets) $
        addError (ConfigurationError $ "Wallet not found: " <> walletName)
    Nothing -> 
      addError (ConfigurationError "Source wallet must be specified")
  
  -- Validate address format
  case toAddress ctx of
    Just addr -> do
      when (T.null (unAddress addr)) $
        addError (InvalidAddress "Destination address cannot be empty")
    Nothing -> 
      addError (ConfigurationError "Destination address must be specified")
  
  -- Validate amount
  case simAmount ctx of
    Just amt -> do
      when (amt == 0) $
        addError (ConfigurationError "Amount cannot be zero")
      when (amt < 1000000) $ -- Minimum 1 ADA
        addError (ConfigurationError "Amount must be at least 1 ADA (1,000,000 lovelace)")
    Nothing -> 
      addError (ConfigurationError "Amount must be specified")

-- | Select optimal UTXOs for transaction
selectUTXOsForTransaction :: SimulationContext -> State SimulationState ()
selectUTXOsForTransaction ctx = do
  case fromWallet ctx of
    Just walletName -> do
      wallets <- gets currentUTXOs
      case Map.lookup walletName wallets of
        Just utxos -> do
          let selected = selectOptimalUTXOs utxos (simAmount ctx)
          modify $ \s -> s { selectedUTXOs = selected }
        Nothing -> return ()
    Nothing -> return ()

-- | Select optimal UTXOs (minimize number of inputs while covering amount + estimated fee)
selectOptimalUTXOs :: [UTXO] -> Maybe Word64 -> [UTXO]
selectOptimalUTXOs utxos maybeAmount = 
  case maybeAmount of
    Just amount -> 
      let estimatedFee = 200000 -- Conservative fee estimate
          totalNeeded = amount + estimatedFee
          sortedUTXOs = sortBy (comparing (\utxo -> lovelace (Types.amount utxo))) (reverse utxos) -- Largest first
          selected = selectUTXOsHelper sortedUTXOs totalNeeded []
      in selected
    Nothing -> []

-- | Helper to select UTXOs
selectUTXOsHelper :: [UTXO] -> Word64 -> [UTXO] -> [UTXO]
selectUTXOsHelper [] _ selected = selected
selectUTXOsHelper (utxo:rest) needed selected =
  let currentTotal = sum $ map (lovelace . Types.amount) selected
      utxoAmount = lovelace (Types.amount utxo)
  in if currentTotal >= needed
     then selected
     else selectUTXOsHelper rest needed (utxo:selected)

-- | Validate transaction feasibility
validateTransactionFeasibility :: SimulationContext -> State SimulationState ()
validateTransactionFeasibility ctx = do
  selected <- gets selectedUTXOs
  case simAmount ctx of
    Just amount -> do
      let totalAvailable = sum $ map (\utxo -> lovelace (Types.amount utxo)) selected
          estimatedFee = 200000 -- Conservative estimate
          totalNeeded = amount + estimatedFee
      
      when (totalAvailable < totalNeeded) $ do
        addError (InsufficientFunds $ 
          "Insufficient funds. Required: " <> T.pack (show totalNeeded) <> 
          " (amount: " <> T.pack (show amount) <> " + fee: " <> T.pack (show estimatedFee) <> 
          "), Available: " <> T.pack (show totalAvailable))
    Nothing -> return ()

-- | Process transaction
processTransaction :: SimulationContext -> State SimulationState ()
processTransaction ctx = do
  -- Create transaction inputs
  inputs <- createTransactionInputs ctx
  
  -- Calculate fees
  fee <- calculateTransactionFee ctx inputs
  
  -- Create transaction outputs
  outputs <- createTransactionOutputs ctx fee
  
  -- Calculate change
  change <- calculateChangeAmount ctx fee
  
  -- Create transaction
  let tx = Transaction
        { txId = generateTransactionId
        , txInputs = inputs
        , txOutputs = outputs
        , txFee = Lovelace fee
        , txValidRange = Nothing
        , txScripts = maybe [] (:[]) (script ctx)
        , txDatums = maybe [] (:[]) (datum ctx)
        , txRedeemers = maybe [] (:[]) (redeemer ctx)
        }
  
  modify $ \s -> s { 
    simTransaction = Just tx
  , changeAmount = change
  }

-- | Create transaction inputs
createTransactionInputs :: SimulationContext -> State SimulationState [TransactionInput]
createTransactionInputs ctx = do
  selected <- gets selectedUTXOs
  return $ map (\utxo -> TransactionInput (txHash utxo) (txIx utxo)) selected

-- | Create transaction outputs
createTransactionOutputs :: SimulationContext -> Word64 -> State SimulationState [TransactionOutput]
createTransactionOutputs ctx fee = do
  case (toAddress ctx, simAmount ctx) of
    (Just addr, Just amt) -> do
      -- Main output
      let mainOutput = TransactionOutput
            { outputAddress = addr
            , outputAmount = Amount { lovelace = amt, assets = Map.empty }
            , outputDatum = datum ctx
            }
      
      -- Change output (if any)
      change <- gets changeAmount
      if change > 0
        then do
          -- Find source wallet address for change
          sourceAddr <- getSourceWalletAddress ctx
          let changeOutput = TransactionOutput
                { outputAddress = sourceAddr
                , outputAmount = Amount { lovelace = change, assets = Map.empty }
                , outputDatum = Nothing
                }
          return [mainOutput, changeOutput]
        else return [mainOutput]
    _ -> return []

-- | Get source wallet address for change
getSourceWalletAddress :: SimulationContext -> State SimulationState Address
getSourceWalletAddress ctx = do
  case fromWallet ctx of
    Just walletName -> do
      -- Find wallet in config
      let walletList = wallets (config ctx)
          maybeWallet = find (\w -> name w == walletName) walletList
      case maybeWallet of
        Just wallet -> return (address wallet)
        Nothing -> return (Address "unknown_address")
    Nothing -> return (Address "unknown_address")

-- | Calculate change amount
calculateChangeAmount :: SimulationContext -> Word64 -> State SimulationState Word64
calculateChangeAmount ctx fee = do
  selected <- gets selectedUTXOs
  case simAmount ctx of
    Just amount -> do
      let totalInput = sum $ map (\utxo -> lovelace (Types.amount utxo)) selected
          totalOutput = amount + fee
          change = if totalInput > totalOutput then totalInput - totalOutput else 0
      return change
    Nothing -> return 0

-- | Calculate transaction fee
calculateTransactionFee :: SimulationContext -> [TransactionInput] -> State SimulationState Word64
calculateTransactionFee ctx inputs = do
  let params = protocolParameters (config ctx)
      baseFee = calculateBaseFee params (length inputs) 2 -- Assume 2 outputs (main + change)
      scriptFee = maybe 0 (calculateScriptFee params) (script ctx)
      totalFee = baseFee + scriptFee
  return totalFee

-- | Update UTXOs after transaction
updateUTXOs :: SimulationContext -> State SimulationState ()
updateUTXOs ctx = do
  case fromWallet ctx of
    Just walletName -> do
      selected <- gets selectedUTXOs
      wallets <- gets currentUTXOs
      
      -- Remove spent UTXOs
      let currentWalletUTXOs = Map.findWithDefault [] walletName wallets
          remainingUTXOs = filter (\utxo -> not (utxo `elem` selected)) currentWalletUTXOs
          updatedWallets = Map.insert walletName remainingUTXOs wallets
      
      modify $ \s -> s { currentUTXOs = updatedWallets }
    Nothing -> return ()

-- | Add error to simulation state
addError :: SimulationError -> State SimulationState ()
addError err = modify $ \s -> s { simErrors = err : simErrors s }

-- | Calculate fees for transaction
calculateFeesForTransaction :: SimulationContext -> SimulationState -> FeeCalculation
calculateFeesForTransaction ctx state = 
  let params = protocolParameters (config ctx)
      baseFee = maybe 0 (calculateBaseFeeForTransaction params) (simTransaction state)
      sizeFee = maybe 0 (calculateSizeFee params) (simTransaction state)
      scriptFee = maybe 0 (calculateScriptFeeForTransaction params) (simTransaction state)
      totalFee = baseFee + sizeFee + scriptFee
  in FeeCalculation
       { baseFee = Lovelace baseFee
       , sizeFee = Lovelace sizeFee
       , scriptFee = Lovelace scriptFee
       , totalFee = Lovelace totalFee
       }

-- | Create simulation details
createSimulationDetails :: SimulationState -> SimulationDetails
createSimulationDetails state = 
  let selected = selectedUTXOs state
      change = changeAmount state
      totalInput = sum $ map (\utxo -> lovelace (Types.amount utxo)) selected
      totalOutput = maybe 0 (sum . map (\o -> lovelace (outputAmount o)) . txOutputs) (simTransaction state)
      fee = maybe 0 (unLovelace . txFee) (simTransaction state)
  in SimulationDetails
       { inputUTXOs = selected
       , outputUTXOs = maybe [] (map createUTXOFromOutput . txOutputs) (simTransaction state)
       , changeUTXO = if change > 0 then Just (createChangeUTXO change) else Nothing
       , totalInputAmount = totalInput
       , totalOutputAmount = totalOutput
       , simChangeAmount = change
       , feeAmount = fee
       }

-- | Create UTXO from transaction output
createUTXOFromOutput :: TransactionOutput -> UTXO
createUTXOFromOutput output = UTXO
  { txHash = generateTransactionId
  , txIx = TxIndex 0
  , amount = outputAmount output
  }

-- | Create change UTXO
createChangeUTXO :: Word64 -> UTXO
createChangeUTXO amount = UTXO
  { txHash = generateTransactionId
  , txIx = TxIndex 1
  , amount = Amount { lovelace = amount, assets = Map.empty }
  }

-- | Validate transaction
validateTransaction :: Transaction -> Either SimulationError ()
validateTransaction tx = do
  -- Check if transaction has inputs
  when (null (txInputs tx)) $
    Left (ConfigurationError "Transaction must have at least one input")
  
  -- Check if transaction has outputs
  when (null (txOutputs tx)) $
    Left (ConfigurationError "Transaction must have at least one output")
  
  -- Check if fee is reasonable
  let fee = unLovelace (txFee tx)
  when (fee < 100000) $ -- Minimum fee
    Left (ConfigurationError "Transaction fee is too low")
  
  when (fee > 10000000) $ -- Maximum reasonable fee
    Left (ConfigurationError "Transaction fee is too high")
  
  return ()

-- | Generate a dummy transaction ID
generateTransactionId :: TransactionId
generateTransactionId = TransactionId "sim_tx_1234567890abcdef"

-- | Calculate base fee for transaction
calculateBaseFeeForTransaction :: ProtocolParameters -> Transaction -> Word64
calculateBaseFeeForTransaction params tx = 
  calculateBaseFee params (length (txInputs tx)) (length (txOutputs tx))

-- | Calculate size fee for transaction
calculateSizeFee :: ProtocolParameters -> Transaction -> Word64
calculateSizeFee params tx = 
  -- Simplified size calculation
  let estimatedSize = fromIntegral (length (txInputs tx) + length (txOutputs tx)) * 100
  in min estimatedSize (maxTxSize params)

-- | Calculate script fee for transaction
calculateScriptFeeForTransaction :: ProtocolParameters -> Transaction -> Word64
calculateScriptFeeForTransaction params tx = 
  sum $ map (calculateScriptFee params) (txScripts tx) 