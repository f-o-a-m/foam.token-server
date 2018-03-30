module Queries.Balance
  ( getBalances
  ) where

import Data.String (fromString)
import qualified Control.Exception as Exception
import Control.Concurrent.Async
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT, ask)
import Network.Ethereum.Web3.Address
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Encoding.Int
import Database.PostgreSQL.Simple (Connection)
import qualified Contracts.ERC20 as ERC20
import Data.Hashable (Hashable(..))
import Data.Typeable
import Haxl.Core
import Types.Application (AppConfig(..), web3Request)
import System.Environment (getEnv)
import Data.Default (def)
import Data.Text (pack)

getBalances
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  => [Address]
  -> m [(Address, Integer)]
getBalances addrs = do
  cfg <- ask
  let st = EthState {appConfig = cfg}
  liftIO $ do
    env <- initEnv (stateSet st stateEmpty) ()
    balances <- runHaxl env $ mapM getBalanceOf addrs
    return $ zipWith (\a b -> (a, unUIntN b)) addrs balances

-- | Request Algebra
data EthReq a where
  BalanceOf :: Address -> EthReq (UIntN 256)
  ExchangedWith :: Address -> EthReq [Address]
  deriving (Typeable)

-- | Boilerplate
deriving instance Eq (EthReq a)
deriving instance Show (EthReq a)

instance Hashable (EthReq a) where
   hashWithSalt s (BalanceOf a) = hashWithSalt s (0::Int, toText a)
   hashWithSalt s (ExchangedWith a) = hashWithSalt s (1::Int, toText a)

-- | The only global state is the ERC20 address
instance StateKey EthReq where
  data State EthReq = EthState {appConfig :: AppConfig}

instance ShowP EthReq where showp = show

instance DataSourceName EthReq where
  dataSourceName _ = "EthDataSource"

instance DataSource u EthReq where
  fetch _state _flags _user bfs = AsyncFetch $ \inner -> do
    asyncs <- mapM (fetchAsync _state) bfs
    inner
    mapM_ wait asyncs

-- Queries
getBalanceOf :: Address -> GenHaxl u (UIntN 256)
getBalanceOf addr = dataFetch (BalanceOf addr)

-- Helpers

fetchAsync
  :: State EthReq
  -> BlockedFetch EthReq
  -> IO (Async ())
fetchAsync _state (BlockedFetch req rvar) =
  async $ do
    e <- Exception.try $ fetchEthReq _state req
    case e of
      Left ex -> putFailure rvar (ex :: Exception.SomeException)
      Right a -> putSuccess rvar a

fetchEthReq
  :: State EthReq
  -> EthReq a
  -> IO a
fetchEthReq EthState{..} (BalanceOf user) = do
  let txOpts = def {callTo = erc20Address appConfig}
  eRes <- web3Request $ ERC20.balanceOf txOpts user
  case eRes of
    Left err -> Exception.throw (error (show err) :: Exception.SomeException)
    Right res -> pure res
fetchEthReq EthState{..} (ExchangedWith user) =
  runReaderT (getExchangePartners user) (pgConn appConfig)

-- Postgres Queries

getExchangePartners
  :: ( MonadReader Connection m
     , MonadIO m
     )
  => Address
  -- ^ address
  -> m [Address]
getExchangePartners = undefined
