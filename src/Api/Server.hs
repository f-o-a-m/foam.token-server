module Api.Server where

import Api.Api
import Data.Swagger (Swagger)
import Servant.Swagger (toSwagger)
import Composite.Record
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Data.String.Conversions (cs)
import Servant
import Servant.Server
import Types.Orphans ()
import Network.Ethereum.Web3.Types
import Network.Wai.Handler.Warp (run)
import qualified Network.Ethereum.Web3.Eth as Eth
import Queries.Transfer
import qualified Types.Transfer as Transfer
import qualified Types.Transaction as Transaction
import Types.Application

getTransfersByTransactionHash
  :: Transaction.FTxHash
  -> AppHandler [Transfer.ApiTransferJson]
getTransfersByTransactionHash txHash = do
  transfers <- getTransfersByHash txHash
  case transfers of
    [] -> throwError err404
    ts -> return ts

getTransfers
  :: Maybe Transaction.FBlockNumber
  -> Maybe Transaction.FBlockNumber
  -> Maybe Transfer.FFrom
  -> AppHandler [Transfer.ApiTransferByBlockJson]
getTransfers mStart mEnd mFrom = do
    start <- maybe (Val . fromInteger <$> getBlockNumber) pure mStart
    end <- maybe (Val . fromInteger <$> getBlockNumber) pure mEnd
    getTransfersInRange start end mFrom
  where
    getBlockNumber :: AppHandler Integer
    getBlockNumber = do
      ebn <- web3Request Eth.blockNumber
      case ebn of
        Left err -> throwError $ err500 {errBody = cs $ show err}
        Right (BlockNumber res) -> pure res

-- | Token server
tokenServer :: ServerT TokenApi AppHandler
tokenServer =
       getTransfersByTransactionHash
  :<|> getTransfers

-- | Swagger
getSwagger :: Swagger
getSwagger = toSwagger tokenApi


-- | Api server
startServer :: IO ()
startServer = do
  cfg <- makeAppConfig
  let server = pure getSwagger :<|> enter (transformAppHandler cfg) tokenServer
  run 9000 $ serve api server
