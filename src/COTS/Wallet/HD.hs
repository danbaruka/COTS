{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | HD Wallet derivation and change address management
module COTS.Wallet.HD
  ( HDWallet (..),
    HDAddress (..),
    ChangeAddress (..),
    deriveAddress,
    deriveChangeAddress,
    rotateChangeAddress,
    validateMnemonic,
    generateMnemonic,
    walletFromMnemonic,
    addressFromBech32,
    bech32FromAddress,
  )
where

import COTS.Types
import Crypto.Hash (SHA256, hashWith)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)

-- | HD Wallet configuration
data HDWallet = HDWallet
  { mnemonic :: [Text],
    passphrase :: Maybe Text,
    accountIndex :: Word32,
    purpose :: Word32, -- CIP-1852 purpose (1852)
    coinType :: Word32, -- ADA = 1815
    changeIndex :: Word32, -- Current change address index
    externalIndex :: Word32, -- Current external address index
    network :: Network
  }

-- | HD Address with derivation path
data HDAddress = HDAddress
  { address :: Address,
    derivationPath :: Text,
    addressIndex :: Word32,
    isChange :: Bool,
    isUsed :: Bool
  }

-- | Change address management
data ChangeAddress = ChangeAddress
  { changeAddress :: Address,
    changeAddressIndex :: Word32,
    lastUsed :: Maybe Word32, -- Slot when last used
    balance :: Word64
  }

-- | Derive address from HD wallet
deriveAddress :: HDWallet -> Word32 -> Bool -> HDAddress
deriveAddress wallet index isChangeAddress =
  let path = buildDerivationPath wallet index isChangeAddress
      address = deriveAddressFromPath wallet path
      derivationPath = T.pack $ show path
   in HDAddress
        { address = address,
          derivationPath = derivationPath,
          addressIndex = index,
          isChange = isChangeAddress,
          isUsed = False
        }

-- | Derive change address
deriveChangeAddress :: HDWallet -> ChangeAddress
deriveChangeAddress wallet =
  let hdAddress = deriveAddress wallet (changeIndex wallet) True
   in ChangeAddress
        { changeAddress = COTS.Wallet.HD.address hdAddress,
          changeAddressIndex = changeIndex wallet,
          lastUsed = Nothing,
          balance = 0
        }

-- | Rotate to next change address
rotateChangeAddress :: HDWallet -> HDWallet
rotateChangeAddress wallet =
  wallet
    { changeIndex = changeIndex wallet + 1
    }

-- | Build derivation path (CIP-1852)
buildDerivationPath :: HDWallet -> Word32 -> Bool -> [Word32]
buildDerivationPath wallet index isChange =
  let change = if isChange then 1 else 0
   in [ purpose wallet,
        coinType wallet,
        accountIndex wallet,
        change,
        index
      ]

-- | Derive address from path (simplified)
deriveAddressFromPath :: HDWallet -> [Word32] -> Address
deriveAddressFromPath wallet path =
  let pathStr = T.intercalate "/" $ map (T.pack . show) path
      -- In a real implementation, this would use proper HD derivation
      -- For now, we'll create a deterministic address from the path
      addressHash = hashWith (undefined :: SHA256) (TE.encodeUtf8 pathStr)
      addressStr = "addr_test1" <> T.pack (show addressHash)
   in Address addressStr

-- | Validate mnemonic (BIP-39)
validateMnemonic :: [Text] -> Bool
validateMnemonic words =
  let wordCount = length words
      validLengths = [12, 15, 18, 21, 24]
   in wordCount `elem` validLengths

-- | Generate mnemonic (placeholder)
generateMnemonic :: Int -> [Text]
generateMnemonic wordCount =
  let validCounts = [12, 15, 18, 21, 24]
   in if wordCount `elem` validCounts
        then replicate wordCount "abandon" -- Placeholder
        else error "Invalid word count"

-- | Create HD wallet from mnemonic
walletFromMnemonic :: [Text] -> Maybe Text -> Network -> HDWallet
walletFromMnemonic mnemonicWords pass network =
  if validateMnemonic mnemonicWords
    then
      HDWallet
        { mnemonic = mnemonicWords,
          passphrase = pass,
          accountIndex = 0,
          purpose = 1852, -- CIP-1852
          coinType = 1815, -- ADA
          changeIndex = 0,
          externalIndex = 0,
          network = network
        }
    else error "Invalid mnemonic"

-- | Convert address to Bech32 (simplified)
bech32FromAddress :: Address -> Text
bech32FromAddress (Address addr) = addr

-- | Convert Bech32 to address (simplified)
addressFromBech32 :: Text -> Maybe Address
addressFromBech32 bech32 =
  if T.isPrefixOf "addr_" bech32
    then Just (Address bech32)
    else Nothing

-- | Get next unused change address
getNextUnusedChangeAddress :: HDWallet -> [ChangeAddress] -> ChangeAddress
getNextUnusedChangeAddress wallet usedAddresses =
  let currentIndex = changeIndex wallet
      isUsed = any (\ca -> changeAddressIndex ca == currentIndex) usedAddresses
   in if isUsed
        then getNextUnusedChangeAddress (rotateChangeAddress wallet) usedAddresses
        else deriveChangeAddress wallet

-- | Get external address
getExternalAddress :: HDWallet -> Word32 -> HDAddress
getExternalAddress wallet index = deriveAddress wallet index False

-- | Get change address
getChangeAddress :: HDWallet -> Word32 -> HDAddress
getChangeAddress wallet index = deriveAddress wallet index True

-- | Update wallet indices
updateWalletIndices :: HDWallet -> Word32 -> Word32 -> HDWallet
updateWalletIndices wallet newExternalIndex newChangeIndex =
  wallet
    { externalIndex = max (externalIndex wallet) newExternalIndex,
      changeIndex = max (changeIndex wallet) newChangeIndex
    }

-- | Check if address belongs to wallet
isAddressFromWallet :: HDWallet -> Address -> Bool
isAddressFromWallet wallet addr =
  let externalAddresses = map (\i -> COTS.Wallet.HD.address (getExternalAddress wallet i)) [0 .. 10]
      changeAddresses = map (\i -> COTS.Wallet.HD.address (getChangeAddress wallet i)) [0 .. 10]
      allAddresses = externalAddresses ++ changeAddresses
   in addr `elem` allAddresses

-- | Get wallet balance across all addresses
getWalletBalance :: HDWallet -> [(Address, Word64)] -> Word64
getWalletBalance wallet addressBalances =
  let walletAddresses =
        map (\i -> COTS.Wallet.HD.address (getExternalAddress wallet i)) [0 .. 10]
          ++ map (\i -> COTS.Wallet.HD.address (getChangeAddress wallet i)) [0 .. 10]
      relevantBalances = filter (\(addr, _) -> addr `elem` walletAddresses) addressBalances
   in sum $ map snd relevantBalances