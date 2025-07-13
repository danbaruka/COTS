{-# LANGUAGE OverloadedStrings #-}

-- | Cardano CLI export functionality
module COTS.Export.CardanoCLI
  ( exportTransactionToCardanoCLI
  , generateCardanoCLICommand
  , generateCardanoCLIArguments
  , exportTransactionToFile
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import qualified Data.Map.Strict as Map

import COTS.Types

-- | Export transaction to Cardano CLI format
exportTransactionToCardanoCLI :: Transaction -> CardanoCLIExport
exportTransactionToCardanoCLI tx = CardanoCLIExport
  { command = "cardano-cli transaction build"
  , arguments = generateCardanoCLIArguments tx
  , description = "Build a Cardano transaction using cardano-cli"
  }

-- | Generate Cardano CLI arguments for transaction
generateCardanoCLIArguments :: Transaction -> [Text]
generateCardanoCLIArguments tx = 
  let inputArgs = map generateInputArg (txInputs tx)
      outputArgs = map generateOutputArg (txOutputs tx)
      feeArg = ["--fee", T.pack (show (unLovelace (txFee tx)))]
      outFileArg = ["--out-file", "tx.raw"]
  in concat [concat inputArgs, concat outputArgs, feeArg, outFileArg]

-- | Generate input argument for Cardano CLI
generateInputArg :: TransactionInput -> [Text]
generateInputArg input = 
  ["--tx-in", T.concat [unTransactionId (inputTxId input), "#", T.pack (show (unTxIndex (inputTxIx input)))]]

-- | Generate output argument for Cardano CLI
generateOutputArg :: TransactionOutput -> [Text]
generateOutputArg output = 
  let addr = unAddress (outputAddress output)
      amount = T.pack (show (lovelace (outputAmount output)))
      datumArg = case outputDatum output of
        Just datum -> ["--tx-out-datum-hash", datumHash datum]
        Nothing -> []
  in concat [["--tx-out", T.concat [addr, "+", amount, " lovelace"]], datumArg]

-- | Generate complete Cardano CLI command
generateCardanoCLICommand :: Transaction -> Text
generateCardanoCLICommand tx = 
  let export = exportTransactionToCardanoCLI tx
      args = T.unwords (arguments export)
  in T.concat [command export, " ", args]

-- | Export transaction to file
exportTransactionToFile :: MonadIO m => Transaction -> FilePath -> m ()
exportTransactionToFile tx filepath = do
  let command = generateCardanoCLICommand tx
  liftIO $ TIO.writeFile filepath command 