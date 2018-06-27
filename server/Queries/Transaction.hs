module Queries.Transaction where

import           Control.Arrow     (returnA)
import           Control.Lens      ((^.))
import           Opaleye           (Column, PGInt8, Query, countRows, desc,
                                    limit, orderBy, queryTable)
import qualified Types.Transaction as Transaction

mostRecentTransactionBlockQuery :: Query (Column PGInt8)
mostRecentTransactionBlockQuery = limit 1 . orderBy (desc id) $ proc () -> do
  transaction <- queryTable Transaction.transactionTable -< ()
  returnA -< transaction ^. Transaction.cBlockNumber

countTransactionsQuery :: Query (Column PGInt8)
countTransactionsQuery = countRows $ queryTable Transaction.transactionTable
