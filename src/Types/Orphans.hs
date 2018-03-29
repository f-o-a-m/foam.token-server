{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types.Orphans where

import Composite.Record ((:->)(Val))
import Data.Int (Int64)
import Data.Proxy (Proxy(Proxy))
import Data.Swagger (ToParamSchema, toParamSchema)
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)

-- Orphan instances for using `s :-> a` as a @Servant.Capture@ or @Servant.QueryParam@
instance ToParamSchema a => ToParamSchema (s :-> a) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy a)
deriving instance ToHttpApiData a => ToHttpApiData (s :-> a)
deriving instance FromHttpApiData a => FromHttpApiData (s :-> a)
