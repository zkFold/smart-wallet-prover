{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.SmartWallet.Orphans () where

import Control.Lens ((?~))
import Control.Monad.IO.Class (MonadIO (..))
import Crypto.PubKey.RSA qualified as RSA
import Crypto.Random.Types qualified as Crypto
import Data.Function ((&))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Servant
import ZkFold.Symbolic.Examples.SmartWallet (ByteStringFromHex (..), ZKF (..), ZKProofBytes (..))

import ZkFold.Cardano.SmartWallet.Utils

instance Crypto.MonadRandom Handler where
  getRandomBytes = liftIO . Crypto.getRandomBytes

instance Swagger.ToSchema RSA.PublicKey where
  declareNamedSchema _ =
    pure $
      Swagger.named "PublicKey" $
        mempty
          & Swagger.description
            ?~ "JSON representation of an RSA public key"

instance Swagger.ToSchema ZKF where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Field element."

instance Swagger.ToSchema ByteStringFromHex where
  declareNamedSchema _ =
    pure $
      Swagger.named "ByteStringFromHex" $
        mempty
          & Swagger.type_
            ?~ Swagger.SwaggerString
          & Swagger.format
            ?~ "hex"
          & Swagger.description
            ?~ "Bytes encoded in hex."

instance Swagger.ToSchema ZKProofBytes where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Proof bytes where bytes are represented in hexadecimal encoding."
