module Api.Api where

import Servant
import qualified Types.Transaction as Transaction
import qualified Types.Transfer as Transfer

type GetTransfersByTransactionHash =
     "transfers"
  :> Capture "transaction_hash" Transaction.FTxHash
  :> Get '[JSON] [Transfer.ApiTransferJson]

type GetTransfers =
     "transfers"
  :> QueryParam "start" Transaction.FBlockNumber
  :> QueryParam "end" Transaction.FBlockNumber
  :> QueryParam "sender" Transaction.FBlockNumber
  :> Get '[JSON] [Transfer.ApiTransferJson]

type TokenApi =
       GetTransfersByTransactionHash
  :<|> GetTransfers
