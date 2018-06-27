module Api.Api where

import           Data.Proxy
import           Network.Ethereum.ABI.Prim.Address
import           Servant
import qualified Types.Transaction                 as Transaction
import qualified Types.Transfer                    as Transfer
import Servant.Swagger.UI


type GetUserBalanceAtBlock =
     "balance"
  :> Capture "userAddress" Address
  :> QueryParam' '[Required] "blockNumber" Integer
  :> Get '[JSON] Integer

type GetTransfersByTransactionHash =
     "transfers"
  :> Capture "transaction_hash" Transaction.FTxHash
  :> Get '[JSON] [(Transaction.ApiTransactionJson, Transfer.ApiTransferJson)]

type GetTransfersBySender =
     "transfers_by_sender"
  :> Capture "sender" Transfer.FFrom
  :> QueryParam "start" Transaction.FBlockNumber
  :> QueryParam "end" Transaction.FBlockNumber
  :> Get '[JSON] [Transfer.ApiTransferByBlockJson]

type GetTransfersByReceiver =
     "transfers_by_receiver"
  :> Capture "receiver" Transfer.FTo
  :> QueryParam "start" Transaction.FBlockNumber
  :> QueryParam "end" Transaction.FBlockNumber
  :> Get '[JSON] [Transfer.ApiTransferByBlockJson]

type GetRichestNeighbors =
     "neighbors"
  :> "richest"
  :> Capture "userAddress" Address
  :> QueryParam "n" Int
  :> QueryParam "blockNumber" Integer
  :> Get '[JSON] [Transfer.ApiBalanceInfoJson]

type GetRichestNeighborsK =
     "neighbors"
  :> "richestK"
  :> Capture "userAddress" Address
  :> QueryParam "nResults" Int
  :> QueryParam' '[Required] "k" Int
  :> QueryParam "blockNumber" Integer
  :> Get '[JSON] [Transfer.ApiBalanceInfoJson]

type TokenApi =
       GetUserBalanceAtBlock
  :<|> GetTransfersByTransactionHash
  :<|> GetTransfersBySender
  :<|> GetTransfersByReceiver
  :<|> GetRichestNeighbors
  :<|> GetRichestNeighborsK

type Api =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
  :<|> TokenApi

tokenApi :: Proxy TokenApi
tokenApi = Proxy

api :: Proxy Api
api = Proxy
