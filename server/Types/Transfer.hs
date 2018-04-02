{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}


module Types.Transfer where

import           Composite
import           Composite.Aeson
import           Composite.Aeson.TH   (makeRecordJsonWrapper)
import           Composite.Opaleye    (defaultRecTable)
import           Composite.Swagger.TH (makeToSchema)
import           Composite.TH         (withLensesAndProxies)
import           Control.Lens.TH      (makeWrapped)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Binary          (encode, decode)
import           Data.Text            (Text)
import           Data.Swagger
import           Opaleye              (Column, PGBytea, PGText, Table (..))
import           Types.Transaction    (CTxHash, FBlockNumber,
                                       FTxHash)
import Data.Proxy
import Opaleye.Internal.RunQuery (QueryRunnerColumnDefault(..), fieldQueryRunnerColumn)
import Opaleye.Constant (Constant(..), constant)
import Data.Profunctor.Product.Default (Default(..))
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- | Token Transfers
--------------------------------------------------------------------------------

newtype Value =
  Value { unValue :: Integer }
    deriving (Eq, Show, Generic, ToJSON, FromJSON, Enum, Num, Ord, Real, Integral)

instance DefaultJsonFormat Value where
  defaultJsonFormat = integralJsonFormat

instance ToSchema Value where
  declareNamedSchema _ = declareNamedSchema (Proxy @Integer)

instance QueryRunnerColumnDefault PGBytea Value where
  queryRunnerColumnDefault = Value . decode <$> fieldQueryRunnerColumn

instance Default Constant Value (Column PGBytea) where
  def = Constant $ constant . encode . unValue

withLensesAndProxies [d|
  type FFrom  = "from"  :-> Text
  type CFrom  = "from"  :-> Column PGText
  type FTo    = "to"    :-> Text
  type CTo    = "to"    :-> Column PGText
  type FValue = "value" :-> Value
  type CValue = "value" :-> Column PGBytea
  type FAddress = "address" :-> Text
  |]

transferTable :: Table (Record DBTransferCols) (Record DBTransferCols)
transferTable = Table "transfers" defaultRecTable

-- | Basic Transfer
type ApiTransfer = '[FTxHash, FFrom, FTo, FValue]
type DBTransfer = '[FTxHash, FFrom, FTo, FValue]
type DBTransferCols = '[CTxHash, CFrom, CTo, CValue]

makeRecordJsonWrapper "ApiTransferJson" ''ApiTransfer
makeWrapped ''ApiTransferJson
makeToSchema "ApiTransferJson" ''ApiTransferJson

-- | Transfer By Block
type ApiTransferByBlock = '[FBlockNumber, FTxHash, FFrom, FTo, FValue]

makeRecordJsonWrapper "ApiTransferByBlockJson" ''ApiTransferByBlock
makeWrapped ''ApiTransferByBlockJson
makeToSchema "ApiTransferByBlockJson" ''ApiTransferByBlockJson

-- | Balance Info
type ApiBalanceInfo = '[FAddress, FValue]

makeRecordJsonWrapper "ApiBalanceInfoJson" ''ApiBalanceInfo
makeWrapped ''ApiBalanceInfoJson
makeToSchema "ApiBalanceInfoJson" ''ApiBalanceInfoJson

