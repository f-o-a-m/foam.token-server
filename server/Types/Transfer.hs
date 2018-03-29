{-# LANGUAGE TemplateHaskell #-}

module Types.Transfer where

import           Composite            ((:->), Record)
import           Composite.Aeson      (DefaultJsonFormat (defaultJsonFormat))
import           Composite.Aeson.TH   (makeRecordJsonWrapper)
import           Composite.Opaleye    (defaultRecTable)
import           Composite.Swagger.TH (makeToSchema)
import           Composite.TH         (withLensesAndProxies)
import           Control.Lens.TH      (makeWrapped)
import           Data.Int             (Int64)
import           Data.Text            (Text)
import           Opaleye              (Column, PGInt8, PGText, Table (..))
import           Types.Transaction    (CBlockNumber, CTxHash, FBlockNumber,
                                       FTxHash)

--------------------------------------------------------------------------------
-- | Token Transfers
--------------------------------------------------------------------------------

withLensesAndProxies [d|
  type FFrom  = "from"  :-> Text
  type CFrom  = "from"  :-> Column PGText
  type FTo    = "to"    :-> Text
  type CTo    = "to"    :-> Column PGText
  type FValue = "value" :-> Int64
  type CValue = "value" :-> Column PGInt8
  |]

transferTable :: Table (Record DBTransferCols) (Record DBTransferCols)
transferTable = Table "transfers" defaultRecTable

-- | Basic Transfer
type ApiTransfer = '[FTxHash, FFrom, FTo, FValue]
type DBTransfer = '[FTxHash, FFrom, FTo, FValue]
type DBTransferCols = '[CTxHash, CFrom, CTo, CValue]

makeRecordJsonWrapper "ApiTransferJson" ''ApiTransfer
makeWrapped ''ApiTransferJson
makeToSchema "ApiTransferJson" ''ApiTransferJson

-- | Transfer By Block
type ApiTransferByBlock = '[FBlockNumber, FTxHash, FFrom, FTo, FValue]

makeRecordJsonWrapper "ApiTransferByBlockJson" ''ApiTransferByBlock
makeWrapped ''ApiTransferByBlockJson
makeToSchema "ApiTransferByBlockJson" ''ApiTransferByBlockJson

