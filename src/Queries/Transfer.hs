module Queries.Transfer where

import Composite.Record (Record, (:->)(Val))
import Control.Arrow (returnA)
import Control.Lens (view, _Unwrapping, (^.))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask)
import Database.PostgreSQL.Simple (Connection)
import Data.Vinyl.Lens (rsubset)
import Opaleye ((.==), (.<=), (.>=), (.&&), runQuery, queryTable, restrict, constant)
import qualified Types.Transfer as Transfer
import qualified Types.Transaction as Transaction

-- | Get all transfers by transaction hash -- possibly more than one exists
getTransfersByHash
  :: ( MonadReader Connection m
     , MonadIO m
     )
  =>  Transaction.FTxHash
  -> m [Transfer.ApiTransferJson]
getTransfersByHash (Val txHash) = do
  conn <- ask
  (transfers :: [Record Transfer.DBTransfer]) <- liftIO . runQuery conn $ proc () -> do
    transfer <- queryTable Transfer.transferTable -< ()
    restrict -< transfer ^. Transaction.cTxHash .== constant txHash
    returnA -< transfer
  pure $ map (view (rsubset . _Unwrapping Transfer.ApiTransferJson)) transfers

-- | Get all transfers from a `sender`.
getTransfersBySender
  :: ( MonadReader Connection m
     , MonadIO m
     )
  =>  Transfer.FFrom
  -> m [Transfer.ApiTransferJson]
getTransfersBySender (Val sender) = do
  conn <- ask
  (transfers :: [Record Transfer.DBTransfer]) <- liftIO . runQuery conn $ proc () -> do
    transfer <- queryTable Transfer.transferTable -< ()
    restrict -< (transfer ^. Transfer.cFrom) .== constant sender
    returnA -< transfer
  pure $ map (view (rsubset . _Unwrapping Transfer.ApiTransferJson)) transfers

-- | Get all transfers in a block range
getTransfersInRange
  :: ( MonadReader Connection m
     , MonadIO m
     )
  => Transaction.FBlockNumber
  -> Transaction.FBlockNumber
  -> m [Transfer.ApiTransferJson]
getTransfersInRange (Val start) (Val end) = do
  conn <- ask
  (transfers :: [Record Transfer.DBTransfer]) <- liftIO . runQuery conn $ proc () -> do
    transfer <- queryTable Transfer.transferTable -< ()
    tx <- queryTable Transaction.transactionTable -< ()
    restrict -< transfer ^. Transaction.cTxHash .== tx ^. Transaction.cTxHash
    restrict -< tx ^. Transaction.cBlockNumber .>= constant start .&& tx ^. Transaction.cBlockNumber .<= constant end
    returnA -< transfer
  pure $ map (view (rsubset . _Unwrapping Transfer.ApiTransferJson)) transfers

{-
  $logInfo "received retrieve request"
  -- Increment the user retrieve requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserRetrieveRequests . appMetrics)

  users <- withDb $ \ conn ->
    runQuery conn . limit 1 $ proc () -> do
      user <- queryTable userTable -< ()
      restrict -< view cId user .== constant userKey
      returnA -< user

  let _ = users :: [Record DbUser]
  case headMay users of
    Just user -> pure $ view (rsubset . _Unwrapping ApiUserJson) user
    Nothing -> throwError err404
-}
