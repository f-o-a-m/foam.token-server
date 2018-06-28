{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}

module Queries.Balance
  ( getBalances
  , getRichestNeighbors
  , getRichestNeighborsK
  ) where

import           Composite.Record
import qualified Contracts.ERC20                   as ERC20
-- import           Control.Concurrent.Async
import qualified Control.Exception                 as Exception
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Reader              (MonadReader, ask,
                                                    runReaderT)
import           Data.Default                      (def)
import           Data.Hashable                     (Hashable (..))
import           Data.Int                          (Int64)
import qualified Data.List                         as L
import           Data.Ord                          (Down (..))
import           Data.Traversable                  (forM)
import           Data.Typeable
import           Haxl.Core
import           Network.Ethereum.ABI.Prim.Address
import           Network.Ethereum.ABI.Prim.Int
import           Network.Ethereum.Web3.Types
import           Queries.Transfer
import           Types.Application                 (AppConfig (..), web3Request)
import Data.Monoid


balanceFlags :: Flags
balanceFlags =
  defaultFlags { trace = 3
               , report = 5
               , caching = 1
               , recording = 0
               }


getBalances
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  => Quantity
  -> [Address]
  -> m [(Address, Integer)]
getBalances bn addrs = do
  cfg <- ask
  let st = EthState {appConfig = cfg}
  liftIO $ do
    e <- initEnv (stateSet st stateEmpty) ()
    balances <- runHaxl e $ mapM (getBalanceOf bn) addrs
    return $ zipWith (\a b -> (a, toInteger b)) addrs balances

getRichestNeighbors
  :: ( MonadReader AppConfig m
     , MonadIO m
     )
  => Quantity
  -> Int
  -> Address
  -> m [(Address, Integer)]
getRichestNeighbors bn n userAddress = do
    (start, end) <- getBlockRange
    cfg <- ask
    let st = EthState {appConfig = cfg}
    liftIO $ do
      e <- initEnv (stateSet st stateEmpty) ()
      runHaxl e {flags = balanceFlags} $ do
        traders <- getTradersInBlockRange (toBN start) (toBN end) userAddress
        pairs <- forM traders $ \trader -> do
          bal <- toInteger <$> getBalanceOf bn trader
          pure (trader, bal)
        let pairs' = filter ((> 0) . snd) pairs
        pure . take n . L.sortOn (Down . snd) $ pairs'
  where
    toBN = fromInteger @Quantity . toInteger


getRichestNeighborsK
  :: (MonadReader AppConfig m, MonadIO m)
  => Quantity
  -> Int
  -> Int
  -> Address
  -> m [(Address, Integer)]
getRichestNeighborsK bn n k userAddress = do
    (start, end) <- getBlockRange
    cfg <- ask
    let st = EthState {appConfig = cfg}
    liftIO $ do
      e <- initEnv (stateSet st stateEmpty) ()
      runHaxl e {flags = balanceFlags} $ do
        neighs <- getAllNeighbors (toBN start) (toBN end) k userAddress
        pure . take n . L.sortOn (Down . snd) $ neighs
  where
    toBN = fromInteger @Quantity . toInteger
    getAllNeighbors :: Quantity -> Quantity -> Int -> Address -> GenHaxl u [(Address, Integer)]
    getAllNeighbors _ _ 0 _ = pure []
    getAllNeighbors s e k' addr = do
      traders <- getTradersInBlockRange s e addr
      pairs <- forM traders $ \t -> do
        bal <- getBalanceOf bn t
        pure (t, toInteger bal)
      let pairs' = filter ((> 0) . snd) pairs
      rest <- concat <$> mapM (getAllNeighbors s e (k'-1)) (map fst pairs')
      pure $ pairs' <> rest

-- | Request Algebra
data EthReq a where
  BalanceOf :: Quantity -> Address -> EthReq (UIntN 256)
  GetTraders :: Quantity -> Quantity -> Address -> EthReq [Address]
  deriving (Typeable)

-- | Boilerplate
deriving instance Eq (EthReq a)
deriving instance Show (EthReq a)

instance Hashable (EthReq a) where
   hashWithSalt s (BalanceOf bn a) = hashWithSalt s (0::Int, show bn, toHexString a)
   hashWithSalt s (GetTraders start end from) = hashWithSalt s (1::Int, show start, show end, toHexString from)

-- | The only global state is the ERC20 address
instance StateKey EthReq where
  data State EthReq = EthState {appConfig :: AppConfig}

instance ShowP EthReq where showp = show

instance DataSourceName EthReq where
  dataSourceName _ = "EthDataSource"

instance DataSource u EthReq where
  fetch _state _flags _user = BackgroundFetch $  mapM_ (fetch' _state)
    --asyncs <- mapM (fetchAsync _state) bfs
    --inner
    --mapM_ wait asyncs
    where
      fetch'
        :: State EthReq
        -> BlockedFetch EthReq
        -> IO ()
      fetch' _state (BlockedFetch req rvar) =
        do
          e <- Exception.try $ fetchEthReq _state req
          case e of
            Left ex -> putFailure rvar (ex :: Exception.SomeException)
            Right a -> putSuccess rvar a

-- Queries
getBalanceOf :: Quantity -> Address -> GenHaxl u (UIntN 256)
getBalanceOf bn addr = dataFetch (BalanceOf bn addr)

getTradersInBlockRange :: Quantity -> Quantity -> Address -> GenHaxl u [Address]
getTradersInBlockRange start end addr = dataFetch (GetTraders start end addr)

-- Helpers



fetchEthReq
  :: State EthReq
  -> EthReq a
  -> IO a
fetchEthReq EthState{..} (BalanceOf bn user) = do
  let txOpts = def { callTo = Just $ erc20Address appConfig
                   }
  eRes <- runReaderT (web3Request $ ERC20.balanceOf txOpts (BlockWithNumber bn) user) appConfig
  case eRes of
    Left err -> Exception.throw (error (show err) :: Exception.SomeException)
    Right res -> pure res
fetchEthReq EthState{..} (GetTraders start end addr) = do
  runReaderT (getTradersInRange addr (Val . toI64 $ start) (Val . toI64 $ end)) appConfig
  where
    toI64 :: Quantity -> Int64
    toI64 (Quantity i) = fromInteger i
