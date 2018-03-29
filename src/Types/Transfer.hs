{-# LANGUAGE TemplateHaskell #-}

module Types.Transfer where

import Composite (Record, (:->))
import Composite.Aeson (DefaultJsonFormat(defaultJsonFormat))
import Composite.Aeson.TH (makeRecordJsonWrapper)
import Composite.Opaleye (defaultRecTable)
import Composite.Swagger.TH (makeToSchema)
import Composite.TH (withLensesAndProxies)
import Control.Lens.TH (makeWrapped)
import Data.Int (Int64)
import Data.Text (Text)
import Opaleye (Column, PGInt8, PGText, Table(..))
import Types.Transaction (FTxHash, CTxHash)

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


type ApiTransfer = '[FTxHash, FFrom, FTo, FValue]
type DBTransfer = '[FTxHash, FFrom, FTo, FValue]
type DBTransferCols = '[CTxHash, CFrom, CTo, CValue]

transferTable :: Table (Record DBTransferCols) (Record DBTransferCols)
transferTable = Table "transfers" defaultRecTable

makeRecordJsonWrapper "ApiTransferJson" ''ApiTransfer
makeWrapped ''ApiTransferJson
makeToSchema "ApiTransferJson" ''ApiTransferJson
