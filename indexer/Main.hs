module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (void)
import Composite.Record
import Control.Monad.Reader (ask)
import Database.PostgreSQL.Simple (Connection)
import Data.Int (Int64)
import Data.Text (pack)
import Queries.Transaction (mostRecentTransactionBlockQuery)
import Types.Application (HttpProvider, makeConnection)
import Types.Transaction as Transaction
import Types.Transfer as Transfer
import Network.Ethereum.Web3
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Encoding.Int (unUIntN)
import Network.Ethereum.Web3.Address (toText, fromText)
import Network.Ethereum.Web3.TH
import Opaleye (constant, runInsertMany, runQuery, count, aggregate)
import System.Environment (lookupEnv, getEnv)
import Data.Binary (encode)


[abiFrom|abis/ERC20.json|]

main :: IO ()
main = do
  conn <- makeConnection
  eaddress <- fromText . pack <$> getEnv "TOKEN_ADDRESS"
  case eaddress of
    Left err -> error err
    Right address -> do
      start <- getStartingBlock conn
      void $ runWeb3 $ eventLoop conn address start

getStartingBlock
  :: Connection
  -> IO BlockNumber
getStartingBlock conn = do
  mstart <- lookupEnv "STARTING_BLOCK"
  case mstart of
    Just bn -> do
      print bn
      return . BlockNumber $ read bn
    Nothing -> do
      print "No Specified Starting Block"
      mLastBlock <- getLastProcessedBlock conn
      case mLastBlock of
        Nothing -> error "No Transfer Transactions found in database, you must specify a Starting Block"
        Just bn -> do
          print $ "Starting Indexer from BlockNumber " ++ show bn
          return $ BlockNumber (toInteger bn)

getLastProcessedBlock
  :: Connection
  -> IO (Maybe Int64)
getLastProcessedBlock conn = do
  (bns :: [Int64]) <- runQuery conn mostRecentTransactionBlockQuery
  case bns of
    [] -> return $ Nothing
    [bn] -> return $ Just bn
    _ -> error "Impossible Query -- got multiple most recent blocks."

eventLoop
  :: Connection
  -> Address
  -> BlockNumber
  -> Web3 HttpProvider ()
eventLoop conn addr start =  do
  let fltr = (eventFilter addr :: Filter Transfer) {filterFromBlock = BlockWithNumber start}
  void $ eventMany' fltr 50 $ \t@Transfer{..} -> do
    change <- ask
    liftIO . print $ "Got Transfer : " ++ show t
    let (BlockNumber bn) = changeBlockNumber change
        txHash = changeTransactionHash change
        logAddress = toText . changeAddress $ change
        value = encode . fromInteger @Int64 . unUIntN $ transferValue_
        (transaction :: Record Transaction.DBTransaction) =  txHash :*: logAddress :*: fromInteger bn :*: RNil
        (transfer :: Record Transfer.DBTransfer) =  txHash :*: toText transferTo_ :*: toText transferFrom_ :*: value :*: RNil
    _ <- liftIO $ runInsertMany conn Transaction.transactionTable [constant transaction]
    _ <- liftIO $ runInsertMany conn Transfer.transferTable [constant transfer]
    return ContinueEvent
