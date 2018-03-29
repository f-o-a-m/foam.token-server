module Api.Api where

import Data.Proxy
import Data.Swagger (Swagger)
import Servant
import qualified Types.Transaction as Transaction
import qualified Types.Transfer as Transfer

type GetTransfersByTransactionHash =
     "transfers"
  :> Capture "transaction_hash" Transaction.FTxHash
  :> Get '[JSON] [Transfer.ApiTransferJson]

type GetTransfersBySender =
     "transfers_by_sender"
  :> Capture "sender" Transfer.FFrom
  :> QueryParam "start" Transaction.FBlockNumber
  :> QueryParam "end" Transaction.FBlockNumber
  :> Get '[JSON] [Transfer.ApiTransferByBlockJson]

type GetSwagger =
     "swagger"
  :> Get '[JSON] Swagger

type TokenApi =
       GetTransfersByTransactionHash
  :<|> GetTransfersBySender

tokenApi :: Proxy TokenApi
tokenApi = Proxy

api :: Proxy (GetSwagger :<|> TokenApi)
api = Proxy
