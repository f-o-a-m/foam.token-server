module Queries.Transfer where

import Types.Application (AppConfig(..))
import Composite.Record
import Control.Arrow (returnA)
import Control.Lens (_Unwrapping, (^.))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask)
import Data.Int (Int64)
import Network.Ethereum.ABI.Prim.Address
import Opaleye as O (Query, Column, PGInt8, (.==), (.<=), (.>=), (.&&), runQuery, queryTable, restrict, constant, orderBy, desc, distinct, min, max, aggregate)
import qualified Types.Transfer as Transfer
import qualified Types.Transaction as Transaction

-- | Get all transfers by transaction hash -- possibly more than one exists
getTransfersByHash
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  =>  Transaction.FTxHash
  -> m [Transfer.ApiTransferJson]
getTransfersByHash (Val txHash) = do
  conn <- pgConn <$> ask
  (transfers :: [Record Transfer.DBTransfer]) <- liftIO . runQuery conn $ proc () -> do
    transfer <- queryTable Transfer.transferTable -< ()
    restrict -< transfer ^. Transaction.cTxHash .== constant txHash
    returnA -< transfer
  pure $ flip map transfers $ \transfer ->
    transfer ^. _Unwrapping Transfer.ApiTransferJson

-- | Get all transfers in a block range based on a `from` account.
getTransfersFromInRange
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  => Transfer.FFrom
  -> Transaction.FBlockNumber
  -> Transaction.FBlockNumber
  -> m [Transfer.ApiTransferByBlockJson]
getTransfersFromInRange mFrom (Val start) (Val end) = do
    conn <- pgConn <$> ask
    let transfersByBlockQ = transfersByBlockQuery start end
        query =  transfersByBlockQ `filterJoinBySender` mFrom
    (transfers :: [(Int64, Record Transfer.DBTransfer)]) <- liftIO . runQuery conn $ query
    let makeTransferWithBlock = \(bn, transfer) ->
          (bn :*: transfer) ^. _Unwrapping Transfer.ApiTransferByBlockJson
    pure $ map makeTransferWithBlock transfers

-- | Get all transfers in a block range based on a `to` account.
getTransfersToInRange
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  => Transfer.FTo
  -> Transaction.FBlockNumber
  -> Transaction.FBlockNumber
  -> m [Transfer.ApiTransferByBlockJson]
getTransfersToInRange receiver (Val start) (Val end) = do
    conn <- pgConn <$> ask
    let transfersByBlockQ = transfersByBlockQuery start end
        query = transfersByBlockQ `filterJoinByReceiver` receiver
    (transfers :: [(Int64, Record Transfer.DBTransfer)]) <- liftIO . runQuery conn $ query
    let makeTransferWithBlock = \(bn, transfer) ->
          (bn :*: transfer) ^. _Unwrapping Transfer.ApiTransferByBlockJson
    pure $ map makeTransferWithBlock transfers

-- | Get all the transfers in a certain block range
transfersByBlockQuery
  :: Int64
  -> Int64
  -> Query (Column PGInt8, Record Transfer.DBTransferCols)
transfersByBlockQuery start end = orderBy (desc fst) $ proc () -> do
  transfer <- queryTable Transfer.transferTable -< ()
  tx <- queryTable Transaction.transactionTable -< ()
  restrict -< transfer ^. Transaction.cTxHash .== tx ^. Transaction.cTxHash
  restrict -< tx ^. Transaction.cBlockNumber .>= constant start .&& tx ^. Transaction.cBlockNumber .<= constant end
  returnA -< (tx ^. Transaction.cBlockNumber, transfer)

-- | get the address of anyone who received tokens in a given block range
allReceiversInRange
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  => Int64
  -> Int64
  -> m [(Address, Integer)]
allReceiversInRange start end = do
  conn <- pgConn <$> ask
  (receivers :: [(Address, Transfer.Value)]) <- liftIO $ runQuery conn $ distinct $ proc () -> do
    (_, transfer) <- transfersByBlockQuery start end -< ()
    returnA -< (transfer ^. Transfer.cTo, transfer ^. Transfer.cValue)
  pure $ map (\(r, v) -> (r, Transfer.unValue v)) receivers

getBlockRange
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  => m (Int64, Int64)
getBlockRange = do
  conn <- pgConn <$> ask
  [mn :: Int64] <- liftIO $ runQuery conn $ aggregate O.min $ proc () -> do
    tx <- queryTable Transaction.transactionTable -< ()
    returnA -< tx ^. Transaction.cBlockNumber
  [mx :: Int64] <- liftIO $ runQuery conn $ aggregate O.max $ proc () -> do
    tx <- queryTable Transaction.transactionTable -< ()
    returnA -< tx ^. Transaction.cBlockNumber
  return (mn, mx)

-- Filters

-- | Filter a transfers query by `from` field
filterBySender
  :: Query (Record Transfer.DBTransferCols)
  -> Transfer.FFrom
  -> Query (Record Transfer.DBTransferCols)
filterBySender query (Val sender) = proc () -> do
  transfer <- query -< ()
  restrict -< transfer ^. Transfer.cFrom .== constant sender
  returnA -< transfer

-- | filter a transfers query by `to` field
filterByReceiver
  :: Query (Record Transfer.DBTransferCols)
  -> Transfer.FTo
  -> Query (Record Transfer.DBTransferCols)
filterByReceiver query (Val receiver) = proc () -> do
  transfer <- query -< ()
  restrict -< transfer ^. Transfer.cTo .== constant receiver
  returnA -< transfer

-- | filter a join on transfers by `from` field
filterJoinBySender
  :: Query (a, Record Transfer.DBTransferCols)
  -> Transfer.FFrom
  -> Query (a, Record Transfer.DBTransferCols)
filterJoinBySender query (Val sender) = proc () -> do
  res@(_, transfer) <- query -< ()
  restrict -< transfer ^. Transfer.cFrom .== constant sender
  returnA -< res

-- | filter a join on transfers by `to` field
filterJoinByReceiver
  :: Query (a, Record Transfer.DBTransferCols)
  -> Transfer.FTo
  -> Query (a, Record Transfer.DBTransferCols)
filterJoinByReceiver query (Val receiver) = proc () -> do
  res@(_, transfer) <- query -< ()
  restrict -< transfer ^. Transfer.cTo .== constant receiver
  returnA -< res
