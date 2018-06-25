module Api.Server where

import Api.Api
import Data.Swagger (Swagger)
import Servant.Swagger (toSwagger)
import Composite.Record
import Control.Lens (_Unwrapping, (^.))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Servant
import Types.Orphans ()
import Network.Ethereum.Web3.Address
import Network.Ethereum.Web3.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Ethereum.Web3.Eth as Eth
import Queries.Transfer
import Queries.Balance
import qualified Types.Transfer as Transfer
import qualified Types.Transaction as Transaction
import Types.Application

-- | get all the transfers for a transaction based on the hash
-- | -- a singler transaction can cause more than one transfer.
getTransfersByTransactionHash
  :: Transaction.FTxHash
  -> AppHandler [Transfer.ApiTransferJson]
getTransfersByTransactionHash = getTransfersByHash

-- | Get all transfers from a certain sender, with the option to specify the range
-- | to within a certain block interval including the endpoints
getTransfersBySender
  :: Transfer.FFrom
  -> Maybe Transaction.FBlockNumber
  -> Maybe Transaction.FBlockNumber
  -> AppHandler [Transfer.ApiTransferByBlockJson]
getTransfersBySender sender mStart mEnd = do
    let start = fromMaybe (Val 0) mStart
    end <- maybe (Val . fromInteger <$> getBlockNumber) pure mEnd
    getTransfersFromInRange sender start end
  where
    getBlockNumber :: AppHandler Integer
    getBlockNumber = do
      ebn <- liftIO $ web3Request Eth.blockNumber
      case ebn of
        Left err -> throwError $ err500 {errBody = cs $ show err}
        Right (BlockNumber res) -> pure res

-- | Get all transfers from a certain sender, with the option to specify the range
-- | to within a certain block interval including the endpoints
getTransfersByReceiver
  :: Transfer.FTo
  -> Maybe Transaction.FBlockNumber
  -> Maybe Transaction.FBlockNumber
  -> AppHandler [Transfer.ApiTransferByBlockJson]
getTransfersByReceiver receiver mStart mEnd = do
    let start = fromMaybe (Val 0) mStart
    end <- maybe (Val . fromInteger <$> getBlockNumber) pure mEnd
    getTransfersToInRange receiver start end
  where
    getBlockNumber :: AppHandler Integer
    getBlockNumber = do
      ebn <- liftIO $ web3Request Eth.blockNumber
      case ebn of
        Left err -> throwError $ err500 {errBody = cs $ show err}
        Right (BlockNumber res) -> pure res

getBalancesBatch
  :: [Text]
  -> AppHandler [Transfer.ApiBalanceInfoJson]
getBalancesBatch addrs = do
  let evalidatedAdders = mapM fromText addrs
  case evalidatedAdders of
    Left err -> throwError err500 {errBody = cs $ show err}
    Right validatedAdders -> do
      balances <- getBalances  validatedAdders
      return . flip map balances $ \(a,b) ->
        (toText a :*: fromInteger b :*: RNil) ^. _Unwrapping Transfer.ApiBalanceInfoJson

getRichestAccounts
  :: Maybe Int
  -> AppHandler [Transfer.ApiBalanceInfoJson]
getRichestAccounts mn = do
  let n = fromMaybe 10 mn
  holders <- getRichestHolders n
  return . flip map holders $ \(a,b) ->
    (toText a :*: fromInteger b :*: RNil) ^. _Unwrapping Transfer.ApiBalanceInfoJson

-- | Token server
tokenServer :: ServerT TokenApi AppHandler
tokenServer =
       getTransfersByTransactionHash
  :<|> getTransfersBySender
  :<|> getTransfersByReceiver
  :<|> getBalancesBatch
  :<|> getRichestAccounts

-- | Swagger
getSwagger :: Swagger
getSwagger = toSwagger tokenApi


-- | Api server
startServer :: IO ()
startServer = do
  cfg <- makeAppConfig
  let server = pure getSwagger :<|> hoistServer tokenApi (transformAppHandler cfg) tokenServer
  run 9000 $
    logStdoutDev $
    serve api server
