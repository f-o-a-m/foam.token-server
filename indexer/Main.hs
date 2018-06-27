module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (void)
import Composite.Record
import Control.Monad.Reader (ask)
import Database.PostgreSQL.Simple (Connection)
import Data.Int (Int64)
import Data.Text (pack)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as T
import Data.ByteArray.Sized (unSizedByteArray)
import Data.ByteArray as BA
import Queries.Transaction (mostRecentTransactionBlockQuery)
import Types.Application (makeConnection)
import Types.Transaction as Transaction
import Types.Transfer as Transfer
import Network.Ethereum.Web3
import Network.Ethereum.Contract.Event
import Network.Ethereum.Web3.Types
import Network.Ethereum.ABI.Prim.Int
import Network.Ethereum.ABI.Prim.Address
import Opaleye (constant, runInsertMany, runQuery, count, aggregate)
import System.Environment (lookupEnv, getEnv)
import Data.Binary (encode)
import qualified Contracts.ERC20 as ERC20
import Control.Exception (try)
import           Data.Default                          (Default, def)

import Data.String (fromString)

main :: IO ()
main = do
  conn <- makeConnection
  address <- fromString <$> getEnv "TOKEN_ADDRESS"
  start <- getStartingBlock conn
  void $ runWeb3 $ eventLoop conn address start

getStartingBlock
  :: Connection
  -> IO Quantity
getStartingBlock conn = do
  mstart <- lookupEnv "STARTING_BLOCK"
  case mstart of
    Just bn -> do
      print bn
      return . Quantity $ read bn
    Nothing -> do
      print ("No Specified Starting Block" :: String)
      mLastBlock <- getLastProcessedBlock conn
      case mLastBlock of
        Nothing -> error "No Transfer Transactions found in database, you must specify a Starting Block"
        Just bn -> do
          print $ "Starting Indexer from BlockNumber " ++ show bn
          return $ Quantity (toInteger bn)

getLastProcessedBlock
  :: Connection
  -> IO (Maybe Int64)
getLastProcessedBlock conn = do
  (bns :: [Int64]) <- runQuery conn mostRecentTransactionBlockQuery
  case bns of
    [] -> return Nothing
    [bn] -> return $ Just bn
    _ -> error "Impossible Query -- got multiple most recent blocks."

eventLoop
  :: Connection
  -> Address
  -> Quantity
  -> Web3 ()
eventLoop conn addr start =  do
  let fltr = (def  :: Filter ERC20.Transfer) { filterAddress = Just [addr]
                                             , filterFromBlock = BlockWithNumber start
                                             }
  void $ eventMany' fltr 50 $ \t@ERC20.Transfer{..} -> do
    change <- ask
    liftIO . print $ "Got Transfer : " ++ show t
    let Just (Quantity bn) = changeBlockNumber change
        Just txHash = T.decodeUtf8 . B16.encode . BA.convert . unSizedByteArray <$> changeTransactionHash change
        logAddress = changeAddress $ change
        value = Transfer.Value . toInteger $ transferValue_
        (transaction :: Record Transaction.DBTransaction) =  txHash :*: logAddress :*: fromInteger bn :*: RNil
        (transfer :: Record Transfer.DBTransfer) =  txHash :*: transferFrom_ :*: transferTo_ :*: value :*: RNil
    _ <- liftIO $ runInsertMany conn Transaction.transactionTable [constant transaction]
    _ <- liftIO $ runInsertMany conn Transfer.transferTable [constant transfer]
    return ContinueEvent
