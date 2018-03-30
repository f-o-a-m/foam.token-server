module Queries.Transfer where

import Types.Application (AppConfig(..))
import Composite.Record
import Control.Arrow (returnA)
import Control.Lens (view, _Unwrapping, (^.), (%~), (#), _1, _2, over, to)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask)
import Data.Binary (decode)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple (Connection)
import Data.Vinyl.Lens (rsubset)
import Opaleye (Query, Column, PGInt8, (.==), (.<=), (.>=), (.&&), runQuery, queryTable, restrict, constant, orderBy, desc)
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
    transfer ^. to Transfer.transferDBToApi . _Unwrapping Transfer.ApiTransferJson

-- | Get all transfers from a `sender`.
getTransfers
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  =>  Transfer.FFrom
  -> m [Transfer.ApiTransferJson]
getTransfers (Val sender) = do
  conn <- pgConn <$> ask
  (transfers :: [Record Transfer.DBTransfer]) <- liftIO . runQuery conn $ proc () -> do
    transfer <- queryTable Transfer.transferTable -< ()
    restrict -< transfer ^. Transfer.cFrom .== constant sender
    returnA -< transfer
  pure $ flip map transfers $ \transfer ->
    transfer ^. to Transfer.transferDBToApi . _Unwrapping Transfer.ApiTransferJson

-- | Get all transfers in a block range
getTransfersInRange
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  => Maybe Transfer.FFrom
  -> Transaction.FBlockNumber
  -> Transaction.FBlockNumber
  -> m [Transfer.ApiTransferByBlockJson]
getTransfersInRange mFrom (Val start) (Val end) = do
    conn <- pgConn <$> ask
    let baseQuery = transfersByBlockQuery start end
        query = maybe baseQuery (filterBySender baseQuery) mFrom
    (transfers :: [(Int64, Record Transfer.DBTransfer)]) <- liftIO . runQuery conn $ query
    let makeTransferWithBlock = \(bn, transfer) ->
          let transfer' = over Transfer.iValue decode transfer
          in (bn :*: Transfer.transferDBToApi transfer) ^. _Unwrapping Transfer.ApiTransferByBlockJson
    pure $ map makeTransferWithBlock transfers

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

filterBySender
  :: Query (a, Record Transfer.DBTransferCols)
  -> Transfer.FFrom
  -> Query (a, Record Transfer.DBTransferCols)
filterBySender query (Val sender) = proc () -> do
  res@(a, transfer) <- query -< ()
  restrict -< transfer ^. Transfer.cFrom .== constant sender
  returnA -< res