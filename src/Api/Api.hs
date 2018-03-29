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

type GetTransfers =
     "transfers"
  :> QueryParam "start" Transaction.FBlockNumber
  :> QueryParam "end" Transaction.FBlockNumber
  :> QueryParam "sender" Transfer.FFrom
  :> Get '[JSON] [Transfer.ApiTransferByBlockJson]

type GetSwagger =
     "swagger"
  :> Get '[JSON] Swagger

type TokenApi =
       GetTransfersByTransactionHash
  :<|> GetTransfers

tokenApi :: Proxy TokenApi
tokenApi = Proxy

api :: Proxy (GetSwagger :<|> TokenApi)
api = Proxy
