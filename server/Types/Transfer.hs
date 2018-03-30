{-# LANGUAGE TemplateHaskell #-}

module Types.Transfer where

import           Composite
import           Composite.Aeson      (DefaultJsonFormat (defaultJsonFormat))
import           Composite.Aeson.TH   (makeRecordJsonWrapper)
import           Composite.Opaleye    (defaultRecTable)
import           Composite.Swagger.TH (makeToSchema)
import           Composite.TH         (withLensesAndProxies)
import           Control.Lens         ((^.), to)
import           Control.Lens.TH      (makeWrapped)
import           Data.Int             (Int64)
import           Data.Text            (Text)
import           Opaleye              (Column, PGBytea, PGText, Table (..))
import           Types.Transaction    (CBlockNumber, CTxHash, FBlockNumber,
                                       FTxHash, fTxHash)
import           Data.ByteString.Lazy (ByteString)
import Data.Binary (decode)

--------------------------------------------------------------------------------
-- | Token Transfers
--------------------------------------------------------------------------------

withLensesAndProxies [d|
  type FFrom  = "from"  :-> Text
  type CFrom  = "from"  :-> Column PGText
  type FTo    = "to"    :-> Text
  type CTo    = "to"    :-> Column PGText
  type FValue = "value" :-> Integer
  type IValue = "value" :-> ByteString
  type CValue = "value" :-> Column PGBytea
  type FAddress = "address" :-> Text
  |]

transferTable :: Table (Record DBTransferCols) (Record DBTransferCols)
transferTable = Table "transfers" defaultRecTable

-- | Basic Transfer
type ApiTransfer = '[FTxHash, FFrom, FTo, FValue]
type DBTransfer = '[FTxHash, FFrom, FTo, IValue]
type DBTransferCols = '[CTxHash, CFrom, CTo, CValue]

makeRecordJsonWrapper "ApiTransferJson" ''ApiTransfer
makeWrapped ''ApiTransferJson
makeToSchema "ApiTransferJson" ''ApiTransferJson

transferDBToApi :: Record DBTransfer -> Record ApiTransfer
transferDBToApi transfer =
      transfer ^. fTxHash
  :*: transfer ^. fFrom
  :*: transfer ^. fTo
  :*: transfer ^. iValue . to decode
  :*: RNil

-- | Transfer By Block
type ApiTransferByBlock = '[FBlockNumber, FTxHash, FFrom, FTo, FValue]

makeRecordJsonWrapper "ApiTransferByBlockJson" ''ApiTransferByBlock
makeWrapped ''ApiTransferByBlockJson
makeToSchema "ApiTransferByBlockJson" ''ApiTransferByBlockJson

-- | Balance Info
type ApiBalanceInfo = '[FAddress, FValue]

makeRecordJsonWrapper "ApiBalanceInfoJson" ''ApiBalanceInfo
makeWrapped ''ApiBalanceInfoJson
makeToSchema "ApiBalanceInfoJson" ''ApiBalanceInfoJson

