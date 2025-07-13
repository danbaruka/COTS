-- | Test suite for COTS
module Main (main) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import COTS.Types
import COTS.Config.Parser
import COTS.Config.Validation
import COTS.Simulation.Core
import COTS.Simulation.Fees
import COTS.Protocol.Parameters

main :: IO ()
main = hspec $ do
  describe "COTS Types" $ do
    it "should create valid protocol parameters" $ do
      let params = defaultProtocolParameters
      minFeeA params `shouldBe` 44
      minFeeB params `shouldBe` 155381

    it "should create valid execution units" $ do
      let units = ExecutionUnits { memory = 1000, steps = 10000 }
      memory units `shouldBe` 1000
      steps units `shouldBe` 10000

  describe "Fee Calculation" $ do
    it "should calculate base fee correctly" $ do
      let params = defaultProtocolParameters
      let fee = calculateBaseFee params 1 1
      fee `shouldSatisfy` (> 0)

    it "should calculate total fee correctly" $ do
      let params = defaultProtocolParameters
      let fee = calculateTotalFee params 1 1 []
      fee `shouldSatisfy` (> 0)

  describe "Protocol Parameters" $ do
    it "should load mainnet parameters" $ do
      let params = loadProtocolParameters Mainnet
      minFeeA params `shouldBe` 44

    it "should load testnet parameters" $ do
      let params = loadProtocolParameters Testnet
      minFeeA params `shouldBe` 44

  describe "Configuration Validation" $ do
    it "should validate valid protocol parameters" $ do
      let params = defaultProtocolParameters
      let errors = validateProtocolParameters params
      errors `shouldBe` []

    it "should detect invalid protocol parameters" $ do
      let invalidParams = defaultProtocolParameters { minFeeA = 0 }
      let errors = validateProtocolParameters invalidParams
      length errors `shouldBe` 1 

  describe "Simulation Logic" $ do
    it "should simulate a simple transaction with correct change and fee" $ do
      let params = defaultProtocolParameters
          alice = Wallet { name = T.pack "alice"
                         , address = Address (T.pack "addr_test1...alice")
                         , utxos = [UTXO (TransactionId (T.pack "tx1")) (TxIndex 0) (Amount 1000000000 mempty)] }
          bob = Wallet { name = T.pack "bob"
                       , address = Address (T.pack "addr_test1...bob")
                       , utxos = [] }
          config = Config { network = Testnet, protocolParameters = params, wallets = [alice, bob] }
          ctx = SimulationContext { config = config
                                 , fromWallet = Just (T.pack "alice")
                                 , toAddress = Just (address bob)
                                 , simAmount = Just 100000000
                                 , script = Nothing
                                 , datum = Nothing
                                 , redeemer = Nothing }
          result = simulateTransaction ctx
      success result `shouldBe` True
      let details = simulationDetails result
      totalInputAmount details `shouldBe` 1000000000
      totalOutputAmount details `shouldBe` 100000000
      simChangeAmount details + feeAmount details + totalOutputAmount details `shouldBe` 1000000000

  describe "CLI Integration" $ do
    it "should parse config and simulate transaction via CLI" $ do
      -- This is a placeholder for a CLI integration test
      -- You can use System.Process to call the CLI binary and check output
      True `shouldBe` True 