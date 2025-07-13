{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Enhanced fee calculation for Cardano transactions
module COTS.Simulation.Fees
  ( calculateBaseFee,
    calculateMinUTxO,
    calculateMultiAssetFee,
    calculateScriptFee,
    calculateTotalFee,
    validateTransactionFees,
    MinUTxOCalculation (..),
    FeeValidationResult (..),
  )
where

import COTS.Protocol.Parameters
import COTS.Types
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

-- | Min-UTxO calculation result
data MinUTxOCalculation = MinUTxOCalculation
  { minUTxOLovelace :: Word64,
    minUTxOAssets :: Word64,
    totalMinUTxO :: Word64,
    calculationDetails :: [(Text, Word64)]
  }

-- | Fee validation result
data FeeValidationResult = FeeValidationResult
  { isValid :: Bool,
    providedFee :: Word64,
    requiredFee :: Word64,
    deficit :: Word64,
    warnings :: [Text],
    errors :: [Text]
  }

-- | Calculate base fee using Cardano's formula
-- fee = (a * size) + b
-- where a = minFeeA, b = minFeeB
calculateBaseFee :: ProtocolParameters -> Word64 -> Word64 -> Word64
calculateBaseFee params txSize numInputs =
  let a = fromIntegral (minFeeA params)
      b = fromIntegral (minFeeB params)
      size = txSize + (numInputs * 2) -- Account for input overhead
   in a * size + b

-- | Calculate minimum UTxO value (Shelley era)
calculateMinUTxO :: ProtocolParameters -> Amount -> MinUTxOCalculation
calculateMinUTxO params amount =
  let baseMinUTxO = 1000000 -- 1 ADA in lovelace
      assetCount = fromIntegral (Map.size (assets amount))
      assetMinUTxO =
        if assetCount > 0
          then assetCount * 1000000 -- 1 ADA per asset
          else 0
      totalMin = baseMinUTxO + assetMinUTxO
   in MinUTxOCalculation
        { minUTxOLovelace = baseMinUTxO,
          minUTxOAssets = assetMinUTxO,
          totalMinUTxO = totalMin,
          calculationDetails =
            [ ("base_min_utxo", baseMinUTxO),
              ("asset_count", assetCount),
              ("asset_min_utxo", assetMinUTxO),
              ("total_min_utxo", totalMin)
            ]
        }

-- | Calculate multi-asset fees
calculateMultiAssetFee :: ProtocolParameters -> [UTXO] -> Word64
calculateMultiAssetFee params inputs =
  let totalAssets = sum $ map (Map.size . assets . amount) inputs
      assetFee = fromIntegral (minFeeA params) * fromIntegral totalAssets
   in assetFee

-- | Calculate script execution fees
calculateScriptFee :: ProtocolParameters -> ExecutionUnits -> Word64
calculateScriptFee params units =
  let memoryFee = fromIntegral (memory units) * 1 -- Fixed price per memory unit
      stepsFee = fromIntegral (steps units) * 1 -- Fixed price per step
   in memoryFee + stepsFee

-- | Calculate total transaction fee
calculateTotalFee :: ProtocolParameters -> Word64 -> Word64 -> [UTXO] -> [ExecutionUnits] -> FeeCalculation
calculateTotalFee params txSize numInputs inputs scriptUnits =
  let baseFee' = calculateBaseFee params txSize numInputs
      multiAssetFee' = calculateMultiAssetFee params inputs
      scriptFee' = sum $ map (calculateScriptFee params) scriptUnits
      totalFee' = baseFee' + multiAssetFee' + scriptFee'
   in FeeCalculation
        { baseFee = Lovelace baseFee',
          sizeFee = Lovelace multiAssetFee',
          scriptFee = Lovelace scriptFee',
          totalFee = Lovelace totalFee'
        }

-- | Validate if provided fee is sufficient
validateTransactionFees :: ProtocolParameters -> Word64 -> Word64 -> Word64 -> [UTXO] -> [ExecutionUnits] -> FeeValidationResult
validateTransactionFees params providedFee txSize numInputs inputs scriptUnits =
  let calculation = calculateTotalFee params txSize numInputs inputs scriptUnits
      requiredFee = unLovelace (totalFee calculation)
      deficit = if providedFee >= requiredFee then 0 else requiredFee - providedFee
      isValid = providedFee >= requiredFee
      warnings = []
      errors = if isValid then [] else [T.pack $ "Insufficient fee: provided " ++ show providedFee ++ ", required " ++ show requiredFee]
   in FeeValidationResult
        { isValid = isValid,
          providedFee = providedFee,
          requiredFee = requiredFee,
          deficit = deficit,
          warnings = warnings,
          errors = errors
        }