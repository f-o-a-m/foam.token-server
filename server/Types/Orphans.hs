{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Types.Orphans where

import           Composite.Record ((:->) (Val))
import           Data.Proxy       (Proxy (Proxy))
import Data.Text (Text)
import           Data.Swagger     (ToParamSchema, toParamSchema, ToSchema(..))
import           Web.HttpApiData  (FromHttpApiData, ToHttpApiData)
import Opaleye.Internal.RunQuery (QueryRunnerColumnDefault(..), fieldQueryRunnerColumn)
import           Opaleye              (PGBytea, Column)
import Network.Ethereum.ABI.Prim.Address
import Composite.Aeson.Formats.Generic
import           Opaleye.Constant                  (Constant (..), constant)
import Composite.Aeson.Formats.Default
import           Data.Profunctor.Product.Default   (Default (..))
import Data.ByteString (ByteString)


-- Orphan instances for using `s :-> a` as a @Servant.Capture@ or @Servant.QueryParam@
instance ToParamSchema a => ToParamSchema (s :-> a) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy a)
deriving instance ToHttpApiData a => ToHttpApiData (s :-> a)
deriving instance FromHttpApiData a => FromHttpApiData (s :-> a)

instance ToParamSchema Address where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema Address where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance QueryRunnerColumnDefault PGBytea Address where
  queryRunnerColumnDefault = (\(Right a) -> a) . fromHexString <$> fieldQueryRunnerColumn @ByteString

instance Default Constant Address (Column PGBytea) where
  def = Constant $ constant . toHexString

instance DefaultJsonFormat Address where
  defaultJsonFormat = aesonJsonFormat
