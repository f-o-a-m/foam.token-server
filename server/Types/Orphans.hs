{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Types.Orphans where

import           Composite.Aeson.Formats.Default
import           Composite.Aeson.Formats.Generic
import           Composite.Record                  ((:->) (Val))
import           Data.ByteString                   (ByteString)
import           Data.Profunctor.Product.Default   (Default (..))
import           Data.Proxy                        (Proxy (Proxy))
import           Data.Swagger                      (ToParamSchema,
                                                    ToSchema (..),
                                                    toParamSchema)
import           Data.Text                         (Text, unpack)
import           Network.Ethereum.ABI.Prim.Address
import           Opaleye                           (Column, PGBytea)
import           Opaleye.Constant                  (Constant (..), constant)
import           Opaleye.Internal.RunQuery         (QueryRunnerColumnDefault (..),
                                                    fieldQueryRunnerColumn)
import           Web.HttpApiData                   (FromHttpApiData (..),
                                                    ToHttpApiData)
import Data.String (fromString)

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

instance FromHttpApiData Address where
  parseQueryParam = Right . fromString . unpack
