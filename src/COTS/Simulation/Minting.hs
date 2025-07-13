{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Native token minting and burning simulation
module COTS.Simulation.Minting
  ( MintingPolicy (..),
    MintingAction (..),
    MintingResult (..),
    validateMintingPolicy,
    simulateMinting,
    simulateBurning,
    calculateMintingFee,
    validateAssetName,
    AssetMetadata (..),
    validateAssetMetadata,
  )
where

import COTS.Protocol.Parameters
import COTS.Types
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

-- | Minting policy configuration
data MintingPolicy = MintingPolicy
  { policyId :: Text,
    policyScript :: Maybe Text, -- Plutus script hash or native script
    policyType :: PolicyType,
    maxSupply :: Maybe Word64,
    currentSupply :: Word64,
    isBurnable :: Bool,
    isTransferable :: Bool
  }

-- | Policy types
data PolicyType
  = NativePolicy -- Native script policy
  | PlutusPolicy -- Plutus script policy
  | MultiSigPolicy -- Multi-signature policy
  deriving (Show, Eq)

-- | Minting action (mint or burn)
data MintingAction
  = Mint
      { mintAssetName :: Text,
        mintQuantity :: Word64,
        mintMetadata :: Maybe AssetMetadata
      }
  | Burn
      { burnAssetName :: Text,
        burnQuantity :: Word64
      }

-- | Minting result
data MintingResult = MintingResult
  { success :: Bool,
    newSupply :: Word64,
    fee :: Word64,
    warnings :: [Text],
    errors :: [Text],
    assetId :: Maybe Text
  }

-- | Asset metadata
data AssetMetadata = AssetMetadata
  { assetName :: Text,
    description :: Maybe Text,
    ticker :: Maybe Text,
    url :: Maybe Text,
    logo :: Maybe Text,
    decimals :: Maybe Word64,
    customFields :: Map.Map Text Value
  }

-- | Validate minting policy
validateMintingPolicy :: MintingPolicy -> [Text]
validateMintingPolicy policy =
  let errors = []
      errors' =
        if T.length (policyId policy) /= 56
          then "Policy ID must be 56 characters" : errors
          else errors
      errors'' = case maxSupply policy of
        Just max ->
          if currentSupply policy > max
            then "Current supply exceeds maximum" : errors'
            else errors'
        Nothing -> errors'
   in errors''

-- | Simulate minting of native tokens
simulateMinting :: ProtocolParameters -> MintingPolicy -> MintingAction -> MintingResult
simulateMinting params policy action = case action of
  Mint assetName quantity metadata -> do
    let errors = validateMintingPolicy policy
        errors' =
          if not (isTransferable policy)
            then "Policy does not allow minting" : errors
            else errors
        errors'' = case maxSupply policy of
          Just max ->
            if currentSupply policy + quantity > max
              then "Minting would exceed maximum supply" : errors'
              else errors'
          Nothing -> errors'
        errors''' = validateAssetName assetName errors''

        success = null errors'''
        newSupply = if success then currentSupply policy + quantity else currentSupply policy
        fee = calculateMintingFee params quantity
        assetId =
          if success
            then Just (policyId policy <> "." <> assetName)
            else Nothing

    MintingResult
      { success = success,
        newSupply = newSupply,
        fee = fee,
        warnings = [],
        errors = errors''',
        assetId = assetId
      }
  Burn assetName quantity -> do
    let errors = validateMintingPolicy policy
        errors' =
          if not (isBurnable policy)
            then "Policy does not allow burning" : errors
            else errors
        errors'' =
          if currentSupply policy < quantity
            then "Insufficient supply to burn" : errors'
            else errors'

        success = null errors''
        newSupply = if success then currentSupply policy - quantity else currentSupply policy
        fee = calculateMintingFee params quantity

    MintingResult
      { success = success,
        newSupply = newSupply,
        fee = fee,
        warnings = [],
        errors = errors'',
        assetId = Just (policyId policy <> "." <> assetName)
      }

-- | Simulate burning of native tokens
simulateBurning :: ProtocolParameters -> MintingPolicy -> Text -> Word64 -> MintingResult
simulateBurning params policy assetName quantity =
  simulateMinting params policy (Burn assetName quantity)

-- | Calculate minting fee
calculateMintingFee :: ProtocolParameters -> Word64 -> Word64
calculateMintingFee params quantity =
  let baseFee = fromIntegral (minFeeA params) * 2 -- Base minting overhead
      quantityFee = fromIntegral (minFeeA params) * fromIntegral quantity `div` 1000000
   in baseFee + quantityFee

-- | Validate asset name
validateAssetName :: Text -> [Text] -> [Text]
validateAssetName name errors =
  let errors' =
        if T.length name > 32
          then "Asset name too long (max 32 characters)" : errors
          else errors
      errors'' =
        if T.any (\c -> c < ' ' || c > '~') name
          then "Asset name contains invalid characters" : errors'
          else errors'
   in errors''

-- | Validate asset metadata
validateAssetMetadata :: AssetMetadata -> [Text]
validateAssetMetadata metadata =
  let errors = []
      errors' =
        if T.length (assetName metadata) > 50
          then "Asset name too long (max 50 characters)" : errors
          else errors
      errors'' = case ticker metadata of
        Just t ->
          if T.length t > 5
            then "Ticker too long (max 5 characters)" : errors'
            else errors'
        Nothing -> errors'
      errors''' = case decimals metadata of
        Just d ->
          if d > 255
            then "Too many decimals (max 255)" : errors''
            else errors''
        Nothing -> errors''
   in errors'''

-- | Create a simple native policy
createNativePolicy :: Text -> MintingPolicy
createNativePolicy pid =
  MintingPolicy
    { policyId = pid,
      policyScript = Nothing,
      policyType = NativePolicy,
      maxSupply = Nothing,
      currentSupply = 0,
      isBurnable = True,
      isTransferable = True
    }

-- | Create a Plutus policy
createPlutusPolicy :: Text -> Text -> MintingPolicy
createPlutusPolicy pid scriptHash =
  MintingPolicy
    { policyId = pid,
      policyScript = Just scriptHash,
      policyType = PlutusPolicy,
      maxSupply = Nothing,
      currentSupply = 0,
      isBurnable = True,
      isTransferable = True
    }