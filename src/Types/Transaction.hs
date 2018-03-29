{-# LANGUAGE TemplateHaskell #-}

module Types.Transaction where

import Composite (Record, (:->))
import Composite.Aeson (DefaultJsonFormat(defaultJsonFormat))
import Composite.Aeson.TH (makeRecordJsonWrapper)
import Composite.Opaleye (defaultRecTable)
import Composite.Swagger.TH (makeToSchema)
import Composite.TH (withLensesAndProxies)
import Control.Lens.TH (makeWrapped)
import Data.Text (Text)
import Opaleye (Column, PGInt8, PGText, Table(..))


--------------------------------------------------------------------------------
-- Raw Transactions
--------------------------------------------------------------------------------

withLensesAndProxies [d|
  type FTxHash      = "transactionHash" :-> Text
  type CTxHash      = "transactionHash" :-> Column PGText
  type FBlockNumber = "blockNumber"     :-> Integer
  type CBlockNumber = "blockNumber"     :-> Column PGInt8
  type FFrom        = "from"            :-> Text
  type CFrom        = "from"            :-> Column PGText
  type FTo          = "to"              :-> Text
  type CTo          = "to"              :-> Column PGText
  |]

type ApiTransaction = '[FTxHash, FFrom, FTo, FBlockNumber]
type DBTransaction = '[FTxHash, CFrom, CTo, CBlockNumber]
type DBTransactionCols = '[CTxHash, CFrom, CTo, CBlockNumber]

transactionTable :: Table (Record DBTransactionCols) (Record DBTransactionCols)
transactionTable = Table "transactions" defaultRecTable

makeRecordJsonWrapper "ApiTransactionJson" ''ApiTransaction
makeWrapped ''ApiTransactionJson
makeToSchema "ApiTransactionJson" ''ApiTransactionJson
