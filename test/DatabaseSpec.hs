{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec (spec) where

import COTS.Database
import COTS.Types
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import System.Directory (doesFileExist, removeFile)
import Test.Hspec
import Data.Word (Word64)

spec :: Spec
spec = do
  describe "Database Operations" $ do
    it "should initialize database successfully" $ do
      let dbPath = "test.db"
      db <- initDatabase dbPath
      closeDatabase db
      exists <- doesFileExist dbPath
      exists `shouldBe` True
      removeFile dbPath

    it "should insert and retrieve UTXOs" $ do
      let dbPath = "test_utxos.db"
      db <- initDatabase dbPath

      -- Create test UTXO
      now <- getCurrentTime
      let testUtxo =
            DBUTXO
              { dbTxHash = "1234567890abcdef",
                dbTxIx = 0,
                dbAddress = "addr_test1qtest",
                dbAmount = 1000000,
                dbAssets = Nothing,
                dbSpent = 0,
                dbCreatedAt = now
              }

      -- Insert UTXO
      insertUTXO db testUtxo

      -- Retrieve UTXOs
      utxos <- getUTXOs db
      closeDatabase db

      -- Verify
      length utxos `shouldBe` 1
      let retrieved = head utxos
      dbTxHash retrieved `shouldBe` "1234567890abcdef"
      dbAmount retrieved `shouldBe` 1000000

      removeFile dbPath

    it "should insert and retrieve wallets" $ do
      let dbPath = "test_wallets.db"
      db <- initDatabase dbPath

      -- Create test wallet
      now <- getCurrentTime
      let testWallet =
            DBWallet
              { dbWalletName = "test_wallet",
                dbWalletAddress = "addr_test1qtest",
                dbWalletCreated = now
              }

      -- Insert wallet
      insertWallet db testWallet

      -- Retrieve wallets
      wallets <- getWallets db
      closeDatabase db

      -- Verify
      length wallets `shouldBe` 1
      let retrieved = head wallets
      dbWalletName retrieved `shouldBe` "test_wallet"
      dbWalletAddress retrieved `shouldBe` "addr_test1qtest"

      removeFile dbPath

    it "should find wallet by name" $ do
      let dbPath = "test_wallet_by_name.db"
      db <- initDatabase dbPath

      -- Create test wallet
      now <- getCurrentTime
      let testWallet =
            DBWallet
              { dbWalletName = "alice",
                dbWalletAddress = "addr_test1qalice",
                dbWalletCreated = now
              }

      -- Insert wallet
      insertWallet db testWallet

      -- Find by name
      mFound <- getWalletByName db "alice"
      mNotFound <- getWalletByName db "bob"
      closeDatabase db

      -- Verify
      mFound `shouldSatisfy` isJust
      mNotFound `shouldSatisfy` isNothing

      case mFound of
        Just found -> do
          dbWalletName found `shouldBe` "alice"
          dbWalletAddress found `shouldBe` "addr_test1qalice"
        Nothing -> expectationFailure "Wallet should be found"

      removeFile dbPath

    it "should insert and retrieve protocol parameters" $ do
      let dbPath = "test_protocol.db"
      db <- initDatabase dbPath

      -- Create test protocol params
      now <- getCurrentTime
      let testParams =
            DBProtocolParams
              { dbParams = "{\"minFeeA\": 44, \"minFeeB\": 155381}",
                dbUpdatedAt = now
              }

      -- Insert protocol params
      insertProtocolParams db testParams

      -- Retrieve protocol params
      params <- getProtocolParams db
      closeDatabase db

      -- Verify
      length params `shouldBe` 1
      let retrieved = head params
      dbParams retrieved `shouldBe` "{\"minFeeA\": 44, \"minFeeB\": 155381}"

      removeFile dbPath

    it "should get latest protocol parameters" $ do
      let dbPath = "test_latest_protocol.db"
      db <- initDatabase dbPath

      -- Create test protocol params
      now <- getCurrentTime
      let oldParams =
            DBProtocolParams
              { dbParams = "{\"minFeeA\": 44, \"minFeeB\": 155381}",
                dbUpdatedAt = now
              }
      let newParams =
            DBProtocolParams
              { dbParams = "{\"minFeeA\": 44, \"minFeeB\": 155381, \"maxTxSize\": 16384}",
                dbUpdatedAt = now
              }

      -- Insert protocol params
      insertProtocolParams db oldParams
      insertProtocolParams db newParams

      -- Get latest
      mLatest <- getLatestProtocolParams db
      closeDatabase db

      -- Verify
      mLatest `shouldSatisfy` isJust
      case mLatest of
        Just latest -> do
          dbParams latest `shouldBe` "{\"minFeeA\": 44, \"minFeeB\": 155381, \"maxTxSize\": 16384}"
        Nothing -> expectationFailure "Latest protocol params should be found"

      removeFile dbPath

    it "should export and import UTXOs correctly" $ do
      let dbPath = "test_export_import.db"
      db <- initDatabase dbPath

      -- Create test UTXOs
      now <- getCurrentTime
      let testUtxo1 =
            UTXO
              (TransactionId "1234567890abcdef")
              (TxIndex 0)
              (Amount 1000000 Map.empty)
      let testUtxo2 =
            UTXO
              (TransactionId "abcdef1234567890")
              (TxIndex 1)
              (Amount 2000000 Map.empty)

      -- Import UTXOs
      importUTXOs db [testUtxo1, testUtxo2]

      -- Export UTXOs
      exported <- exportUTXOs db
      closeDatabase db

      -- Verify
      length exported `shouldBe` 2
      let amounts = map (lovelace . amount) exported
      amounts `shouldContain` [1000000 :: Word64]
      amounts `shouldContain` [2000000 :: Word64]

      removeFile dbPath

    it "should provide accurate database statistics" $ do
      let dbPath = "test_stats.db"
      db <- initDatabase dbPath

      -- Add some test data
      now <- getCurrentTime

      -- Add UTXO
      let testUtxo =
            DBUTXO
              { dbTxHash = "1234567890abcdef",
                dbTxIx = 0,
                dbAddress = "addr_test1qtest",
                dbAmount = 1000000,
                dbAssets = Nothing,
                dbSpent = 0,
                dbCreatedAt = now
              }
      insertUTXO db testUtxo

      -- Add wallet
      let testWallet =
            DBWallet
              { dbWalletName = "test_wallet",
                dbWalletAddress = "addr_test1qtest",
                dbWalletCreated = now
              }
      insertWallet db testWallet

      -- Add protocol params
      let testParams =
            DBProtocolParams
              { dbParams = "{\"minFeeA\": 44}",
                dbUpdatedAt = now
              }
      insertProtocolParams db testParams

      -- Get statistics
      stats <- inspectDatabase db
      closeDatabase db

      -- Verify statistics contain expected information
      stats `shouldContain` "UTXOs (unspent): 1"
      stats `shouldContain` "Wallets: 1"
      stats `shouldContain` "Protocol Parameters: 1"
      stats `shouldContain` "Total lovelace: 1000000"

      removeFile dbPath

    it "should handle database reset correctly" $ do
      let dbPath = "test_reset.db"

      -- Create database with data
      db <- initDatabase dbPath
      now <- getCurrentTime
      let testUtxo =
            DBUTXO
              { dbTxHash = "1234567890abcdef",
                dbTxIx = 0,
                dbAddress = "addr_test1qtest",
                dbAmount = 1000000,
                dbAssets = Nothing,
                dbSpent = 0,
                dbCreatedAt = now
              }
      insertUTXO db testUtxo
      closeDatabase db

      -- Verify data exists
      db2 <- initDatabase dbPath
      utxos <- getUTXOs db2
      closeDatabase db2
      length utxos `shouldBe` 1

      -- Reset database
      resetDatabase dbPath

      -- Verify database is empty
      db3 <- initDatabase dbPath
      utxos2 <- getUTXOs db3
      closeDatabase db3
      length utxos2 `shouldBe` 0

      removeFile dbPath
  where
    isJust (Just _) = True
    isJust Nothing = False
    isNothing Nothing = True
    isNothing (Just _) = False