{-# LANGUAGE NamedFieldPuns #-}

module Types.Application
  ( AppHandler
  , AppConfig(..)
  , Web3Config(..)
  , makeWeb3Config
  , makeAppConfig
  , transformAppHandler
  , makeConnection
  ) where

import           Control.Monad.Except           (ExceptT, MonadError)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (MonadReader, ReaderT,
                                                 runReaderT)
import           Data.String                    (fromString)
import           Database.PostgreSQL.Simple     (ConnectInfo (..), Connection,
                                                 connect)
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.ABI.Prim.Address
import           Servant                        (ServantErr)
import           Servant.Server                 (Handler (..))
import           System.Environment             (getEnv)
import           Network.HTTP.Client               (Manager,
                                                    newManager)
import           Network.HTTP.Client.TLS           (tlsManagerSettings)


data Web3Config =
  Web3Config { manager  :: Manager
             , provider :: Provider
             }

makeWeb3Config :: IO Web3Config
makeWeb3Config = do
  mgr <- liftIO $ newManager tlsManagerSettings
  url <- getEnv "NODE_URL"
  pure $ Web3Config mgr (Provider (HttpProvider url) Nothing)

-- | App Config
data AppConfig =
  AppConfig { pgConn       :: Connection
            , web3 :: Web3Config
            , erc20Address :: Address
            }

makeConnection :: IO Connection
makeConnection = do
  connInfo <- do
      host <- getEnv "PGHOST"
      port <- read <$> getEnv "PGPORT"
      user <- getEnv "PGUSER"
      pw <- getEnv "PGPASSWORD"
      db <- getEnv "PGDATABASE"
      pure $ ConnectInfo host port user pw db
  connect connInfo

makeAppConfig :: IO AppConfig
makeAppConfig = do
  pg <- makeConnection
  addr <- fromString <$> getEnv "TOKEN_ADDRESS"
  web3 <- makeWeb3Config
  pure AppConfig { pgConn = pg
                 , web3
                 , erc20Address = addr
                 }

newtype AppHandler a =
  AppHandler {runAppHandler :: ReaderT AppConfig (ExceptT ServantErr IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError ServantErr, MonadReader AppConfig)

transformAppHandler
  :: AppConfig
  -> (forall a. AppHandler a -> Handler a)
transformAppHandler cfg = Handler . flip runReaderT cfg . runAppHandler
