module Types.Application
  ( AppHandler
  , AppConfig(..)
  , HttpProvider
  , makeAppConfig
  , transformAppHandler
  , web3Request
  , makeConnection
  ) where

import           Control.Monad.Except           (ExceptT, MonadError)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (MonadReader, ReaderT,
                                                 runReaderT)
import           Control.Natural                (wrapNT)
import           Database.PostgreSQL.Simple     (ConnectInfo (..), Connection,
                                                 connect)
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types    (Web3, Web3Error)
import           Servant                        (ServantErr)
import           Servant.Server                 ((:~>) (..), Handler (..))
import           System.Environment             (getEnv)
import           System.IO.Unsafe               (unsafePerformIO)


-- | Web3 Config
data HttpProvider

instance Provider HttpProvider where
  rpcNode = return httpProvider

httpProvider :: RPCNode
httpProvider = unsafePerformIO $ do
  uri <- getEnv "NODE_URL"
  makeRPCNode uri
{-# NOINLINE httpProvider #-}

web3Request
  :: Web3 HttpProvider a
  -> AppHandler (Either Web3Error a)
web3Request action = liftIO $ runWeb3 action

-- | App Config
newtype AppConfig =
  AppConfig { pgConn :: Connection
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
  pure AppConfig {pgConn = pg}

newtype AppHandler a =
  AppHandler {runAppHandler :: ReaderT AppConfig (ExceptT ServantErr IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError ServantErr, MonadReader AppConfig)

transformAppHandler
  :: AppConfig
  -> AppHandler :~> Handler
transformAppHandler cfg = wrapNT $ Handler . flip runReaderT cfg . runAppHandler
