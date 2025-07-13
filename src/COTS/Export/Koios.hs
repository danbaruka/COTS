{-# LANGUAGE OverloadedStrings #-}

-- | Koios API export functionality
module COTS.Export.Koios
  ( exportTransactionToKoios
  , generateKoiosRequest
  , generateKoiosJSON
  , exportTransactionToKoiosFile
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import COTS.Types

-- | Export transaction to Koios API format
exportTransactionToKoios :: Transaction -> KoiosExport
exportTransactionToKoios tx = KoiosExport
  { endpoint = "/api/v1/submit/tx"
  , method = "POST"
  , headers = Map.fromList 
    [ ("Content-Type", "application/json")
    , ("Accept", "application/json")
    ]
  , body = generateKoiosJSON tx
  }

-- | Generate Koios API request
generateKoiosRequest :: Transaction -> Text
generateKoiosRequest tx = 
  let export = exportTransactionToKoios tx
      headersText = T.unlines $ map (\(k, v) -> k <> ": " <> v) (Map.toList (headers export))
  in T.concat 
    [ method export, " ", endpoint export, " HTTP/1.1\n"
    , headersText, "\n"
    , body export
    ]

-- | Generate Koios JSON payload
generateKoiosJSON :: Transaction -> Text
generateKoiosJSON tx = 
  let txHex = encodeTransactionToHex tx
      json = T.concat 
        [ "{\n"
        , "  \"tx\": \"", txHex, "\"\n"
        , "}"
        ]
  in json

-- | Encode transaction to hex (simplified for simulation)
encodeTransactionToHex :: Transaction -> Text
encodeTransactionToHex tx = 
  -- In a real implementation, this would serialize the transaction to CBOR
  -- For simulation, we'll create a placeholder hex string
  let txIdStr = unTransactionId (txId tx)
      fee = T.pack (show (unLovelace (txFee tx)))
  in T.concat ["simulated_tx_", txIdStr, "_fee_", fee]

-- | Export transaction to Koios file
exportTransactionToKoiosFile :: MonadIO m => Transaction -> FilePath -> m ()
exportTransactionToKoiosFile tx filepath = do
  let request = generateKoiosRequest tx
  liftIO $ TIO.writeFile filepath request 