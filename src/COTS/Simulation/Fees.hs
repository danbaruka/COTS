{-# LANGUAGE OverloadedStrings #-}

-- | Fee calculation for Cardano transactions
module COTS.Simulation.Fees
  ( calculateBaseFee
  , calculateScriptFee
  , calculateTotalFee
  , estimateTransactionSize
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

import COTS.Types

-- | Calculate base fee for transaction
calculateBaseFee :: ProtocolParameters -> Int -> Int -> Word64
calculateBaseFee params inputCount outputCount = 
  let size = estimateTransactionSize inputCount outputCount
      fee = minFeeA params * fromIntegral size + minFeeB params
  in fee

-- | Calculate script execution fee
calculateScriptFee :: ProtocolParameters -> PlutusScript -> Word64
calculateScriptFee params script = 
  -- Simplified script fee calculation
  -- In reality, this would depend on execution units
  let baseScriptFee = 1000000 -- 1 ADA base fee for scripts
      scriptSize = T.length (scriptBytes script)
      sizeMultiplier = fromIntegral scriptSize `div` 1000
  in baseScriptFee + (sizeMultiplier * 100000)

-- | Calculate total fee for transaction
calculateTotalFee :: ProtocolParameters -> Int -> Int -> [PlutusScript] -> Word64
calculateTotalFee params inputCount outputCount scripts = 
  let baseFee = calculateBaseFee params inputCount outputCount
      scriptFees = sum $ map (calculateScriptFee params) scripts
  in baseFee + scriptFees

-- | Estimate transaction size
estimateTransactionSize :: Int -> Int -> Word64
estimateTransactionSize inputCount outputCount = 
  let inputSize = fromIntegral inputCount * 64  -- Approximate size per input
      outputSize = fromIntegral outputCount * 128 -- Approximate size per output
      headerSize = 256 -- Transaction header size
  in headerSize + inputSize + outputSize

-- | Calculate fee for UTXO size
calculateUTXOFee :: ProtocolParameters -> Word64 -> Word64
calculateUTXOFee params utxoSize = 
  let feePerByte = coinsPerUtxoSize params
  in utxoSize * feePerByte

-- | Calculate collateral fee
calculateCollateralFee :: ProtocolParameters -> Int -> Word64
calculateCollateralFee params collateralInputs = 
  let maxCollateral = maxCollateralInputs params
      collateralPercent = collateralPercentage params
      baseCollateralFee = 2000000 -- 2 ADA base collateral
  in if collateralInputs > fromIntegral maxCollateral
     then baseCollateralFee * 2 -- Double fee if too many collateral inputs
     else baseCollateralFee 