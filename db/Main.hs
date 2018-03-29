{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Int (Int64)
import Database.PostgreSQL.Simple (Connection, execute_)
import Database.PostgreSQL.Simple.SqlQQ
import Types.Application (makeConnection)

main :: IO ()
main = do
  conn <- makeConnection
  mapM_ ($ conn) [ createTransactionsTable
                 , createTransfersTable
                 ]

createTransactionsTable :: Connection -> IO Int64
createTransactionsTable conn = execute_ conn q
  where
    q = [sql|
            CREATE TABLE transactions (
              id SERIAL8 PRIMARY KEY UNIQUE,
              transactionHash varchar not null,
              blockNumber integer not null,
              from varchar not null,
              to varchar not null
            );
        |]

createTransfersTable :: Connection -> IO Int64
createTransfersTable conn = execute_ conn q
  where
    q = [sql|
            CREATE TABLE transfers (
              id SERIAL8 PRIMARY KEY UNIQUE,
              transactionHash varchar not null,
              from varchar not null,
              to varchar not null,
              value integer not null
            );
        |]
