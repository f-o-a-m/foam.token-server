module Types.Application
  ( AppHandler
  , AppConfig(..)
  , makeAppConfig
  , transformAppHandler
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Natural (wrapNT)
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..), connect)
import Servant (ServantErr)
import Servant.Server ((:~>)(..), Handler(..))
import System.Environment (getEnv)


newtype AppConfig =
  AppConfig { pgConn :: Connection
            }

makeConnection :: IO Connection
makeConnection = do
  connInfo <- do
      host <- getEnv "PG_HOST"
      port <- read <$> getEnv "PG_PORT"
      user <- getEnv "PG_USER"
      pw <- getEnv "PG_PASSWORD"
      db <- getEnv "PG_DB"
      pure $ ConnectInfo host port user pw db
  connect connInfo

makeAppConfig :: IO AppConfig
makeAppConfig = do
  pg <- makeConnection
  pure $ AppConfig {pgConn = pg}

newtype AppHandler a =
  AppHandler {runAppHandler :: ReaderT AppConfig (ExceptT ServantErr IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError ServantErr, MonadReader AppConfig)

transformAppHandler
  :: AppConfig
  -> AppHandler :~> Handler
transformAppHandler cfg = wrapNT $ Handler . flip runReaderT cfg . runAppHandler
