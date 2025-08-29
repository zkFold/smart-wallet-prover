module ZkFold.Cardano.SmartWallet.Types.Common (
  ProveRequestMonad,
  addFieldDescription,
  addDescription,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Crypto.Random.Types qualified as Crypto
import Servant
import Data.Swagger qualified as Swagger
import Data.Text (Text)

import Debug.Trace (trace)

type ProveRequestMonad m = (MonadError ServerError m, MonadIO m, Crypto.MonadRandom m)

-- | In a given Swagger Schema, add description "desc" for the field named @name@
-- 
addFieldDescription :: Text -> Text -> Swagger.Schema -> Swagger.Schema
addFieldDescription name desc s = trace (show $ Swagger._schemaProperties s) $ 
  s { Swagger._schemaProperties =
       InsOrd.adjust (fmap (\sub -> sub { Swagger._schemaDescription = Just desc })) name $ Swagger._schemaProperties s
  }

addDescription :: Text -> Swagger.Schema -> Swagger.Schema
addDescription desc s = s {Swagger._schemaDescription = Just desc}
