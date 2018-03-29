module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (void)
import Composite.Record
import Control.Monad.Reader (ask)
import Database.PostgreSQL.Simple (Connection)
import Types.Application (HttpProvider)
import Types.Transaction as Transaction
import Types.Transfer as Transfer
import Network.Ethereum.Web3
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Encoding.Int (unUIntN)
import Network.Ethereum.Web3.Address (toText)
import Network.Ethereum.Web3.TH
import Opaleye (constant, runInsertMany)

[abiFrom|abis/ERC20.json|]

main :: IO ()
main = return ()

eventLoop :: Connection -> Address -> Web3 HttpProvider ()
eventLoop conn addr =  do
  let fltr = eventFilter addr
  void $ event fltr $ \t@Transfer{..} -> do
    change <- ask
    liftIO . print $ "Got Transfer : " ++ show t
    let (BlockNumber bn) = changeBlockNumber change
        txHash = changeTransactionHash change
        logAddress = toText . changeAddress $ change
        value = fromInteger . unUIntN $ transferValue_
        (transaction :: Record Transaction.DBTransaction) =  txHash :*: logAddress :*: fromInteger bn :*: RNil
        (transfer :: Record Transfer.DBTransfer) =  txHash :*: toText transferTo_ :*: toText transferFrom_ :*: value :*: RNil
    void $ liftIO $ runInsertMany conn Transaction.transactionTable [constant transaction]
    void $ liftIO $ runInsertMany conn Transfer.transferTable [constant transfer]
    return ContinueEvent
