module Queries.Balance
  ( getBalances
  , getRichestHolders
  ) where

import Data.Ord (Down(..))
import Control.Monad (join)
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
import Queries.Transfer (allReceiversInRange, partitionBlockRange)
import System.Environment (getEnv)
import Data.Default (def)
import Data.Text (pack)
import qualified Haxl.Prelude as HP
import Data.Int (Int64)
import Control.Arrow
import qualified Data.List as L


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
    balances <- runHaxl env $ HP.mapM getBalanceOf addrs
    return $ zipWith (\a b -> (a, unUIntN b)) addrs balances

getRichestHolders
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  => Int
  -> m [(Address, Integer)]
getRichestHolders n = do
    startEnds <- partitionBlockRange 50
    cfg <- ask
    let st = EthState {appConfig = cfg}
    liftIO $ do
      env <- initEnv (stateSet st stateEmpty) ()
      pairs <- runHaxl env $ HP.forM startEnds $ \(start, end) -> do
        receivers <- getReceiversInBlockRange start end
        balances <- HP.mapM getBalanceOf $ receivers
        return $ zipWith (\a b -> (a, unUIntN b)) receivers balances
      return . take n . L.sortOn (Down . snd) . L.nub . join $ pairs

-- | Request Algebra
data EthReq a where
  BalanceOf :: Address -> EthReq (UIntN 256)
  GetReceivers :: Int64 -> Int64 -> EthReq [(Address, Integer)]
  deriving (Typeable)

-- | Boilerplate
deriving instance Eq (EthReq a)
deriving instance Show (EthReq a)

instance Hashable (EthReq a) where
   hashWithSalt s (BalanceOf a) = hashWithSalt s (0::Int, toText a)
   hashWithSalt s (GetReceivers start end) = hashWithSalt s (1::Int, start, end)

-- | The only global state is the ERC20 address
instance StateKey EthReq where
  data State EthReq = EthState {appConfig :: AppConfig}

instance ShowP EthReq where showp = show

instance DataSourceName EthReq where
  dataSourceName _ = "EthDataSource"

instance DataSource u EthReq where
  fetch _state _flags _user bfs = AsyncFetch $ \inner -> do
    asyncs <- HP.mapM (fetchAsync _state) bfs
    inner
    mapM_ wait asyncs

-- Queries
getBalanceOf :: Address -> GenHaxl u (UIntN 256)
getBalanceOf addr = dataFetch (BalanceOf addr)

getReceiversInBlockRange :: Int64 -> Int64 -> GenHaxl u [Address]
getReceiversInBlockRange start end = map fst <$> dataFetch (GetReceivers start end)

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
fetchEthReq EthState{..} (GetReceivers start end) =
  runReaderT (allReceiversInRange start end) appConfig
