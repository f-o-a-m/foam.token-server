module Queries.Balance
  ( getBalances
  ) where

import Data.String (fromString)
import Control.Exception (SomeException)
import Control.Concurrent.Async
import Network.Ethereum.Web3.Address
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Encoding.Int
import qualified Contracts.ERC20 as ERC20
import Data.Hashable (Hashable(..))
import Data.Typeable
import Haxl.Core
import Types.Application (web3Request)
import System.Environment (getEnv)
import Data.Default (def)
import Data.Text (pack)

getBalances :: [Address] -> IO [(Address, Integer)]
getBalances addrs = do
  st <- initGlobalState
  env <- initEnv (stateSet st stateEmpty) ()
  balances <- runHaxl env $ mapM getBalanceOf addrs
  return $ zipWith (\a b -> (a, unUIntN b)) addrs balances

-- | Request Algebra
data EthReq a where
  BalanceOf :: Address -> EthReq (UIntN 256)
  deriving (Typeable)

-- | Boilerplate
deriving instance Eq (EthReq a)
deriving instance Show (EthReq a)

instance Hashable (EthReq a) where
   hashWithSalt s (BalanceOf a) = hashWithSalt s (0::Int, toText a)

-- | The only global state is the ERC20 address
instance StateKey EthReq where
  data State EthReq = EthState {erc20Address :: Address}

initGlobalState :: IO (State EthReq)
initGlobalState = do
  addr <- getEnv "TOKEN_ADDRESS"
  return EthState { erc20Address = fromString addr
                  }

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
    e <- fetchEthReq _state req
    case e of
      Left ex -> putFailure rvar (error (show ex) :: SomeException)
      Right a -> putSuccess rvar a

fetchEthReq
  :: State EthReq
  -> EthReq a
  -> IO (Either Web3Error a)
fetchEthReq EthState{..} (BalanceOf user) = do
  let txOpts = def {callTo = erc20Address}
  web3Request $ ERC20.balanceOf txOpts user
