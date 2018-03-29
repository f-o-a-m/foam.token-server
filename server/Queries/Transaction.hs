module Queries.Transaction where

import Control.Arrow (returnA)
import Control.Lens ((^.))
import Opaleye (Query, Column, PGInt8, orderBy, desc, limit, queryTable, countRows)
import qualified Types.Transaction as Transaction

mostRecentTransactionBlockQuery :: Query (Column PGInt8)
mostRecentTransactionBlockQuery = limit 1 . orderBy (desc id) $ proc () -> do
  transaction <- queryTable Transaction.transactionTable -< ()
  returnA -< transaction ^. Transaction.cBlockNumber

countTransactionsQuery :: Query (Column PGInt8)
countTransactionsQuery = countRows $ queryTable Transaction.transactionTable
