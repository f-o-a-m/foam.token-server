{-# LANGUAGE TemplateHaskell #-}

module Types where

import Composite (Record, (:->))
import Composite.Opaleye (defaultRecTable)
import Composite.TH (withLensesAndProxies)
import Data.Text (Text)
import Opaleye (Column, PGInt8, PGText, Table(..))

--------------------------------------------------------------------------------
-- | Token Transfers
--------------------------------------------------------------------------------

withLensesAndProxies [d|
  type FTxHash      = "tx_hash"      :-> Text
  type CTxHash      = "tx_hash"      :-> Column PGText
  type FBlockNumber = "block_number" :-> Integer
  type CBlockNumber = "block_number" :-> Column PGInt8
  type FAddress     = "address"      :-> Text
  type CAddress     = "address"      :-> Column PGText
  type FValue       = "value"        :-> Integer
  type CValue       = "value"        :-> Column PGInt8
  |]

type ApiTransfer = '[FTxHash, FAddress, FAddress, FValue]
type DBTransfer = '[FTxHash, CAddress, CAddress, CValue]
type DBTransferCols = '[CTxHash, CBlockNumber, CAddress, CAddress, CAddress]


userTable :: Table (Record DBTransferCols) (Record DBTransferCols)
userTable = Table "users" defaultRecTable
