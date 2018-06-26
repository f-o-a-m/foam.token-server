{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Types.Orphans where

import           Composite.Record ((:->) (Val))
import           Data.Proxy       (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Swagger     (ToParamSchema, toParamSchema, ToSchema(..))
import           Web.HttpApiData  (FromHttpApiData, ToHttpApiData)
import Opaleye.Internal.RunQuery (QueryRunnerColumnDefault(..), fieldQueryRunnerColumn)
import           Opaleye              (PGText)
import Network.Ethereum.ABI.Prim.Address
import Composite.Aeson.Formats.Generic

import Composite.Aeson.Formats.Default


-- Orphan instances for using `s :-> a` as a @Servant.Capture@ or @Servant.QueryParam@
instance ToParamSchema a => ToParamSchema (s :-> a) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy a)
deriving instance ToHttpApiData a => ToHttpApiData (s :-> a)
deriving instance FromHttpApiData a => FromHttpApiData (s :-> a)

instance ToParamSchema Address where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema Address where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance QueryRunnerColumnDefault PGText Address where
  queryRunnerColumnDefault = (\(Right a) -> a) . fromHexString . T.encodeUtf8 <$> fieldQueryRunnerColumn @Text

instance DefaultJsonFormat Address where
  defaultJsonFormat = aesonJsonFormat
