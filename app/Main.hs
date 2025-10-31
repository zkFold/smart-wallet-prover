{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Crypto.Random.Types qualified as Crypto
import Data.ByteString (ByteString)
import Data.Functor.Rep (tabulate)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Data.OpenApi as Swagger hiding (value, info)
import Data.Yaml (FromJSON)
import Data.Yaml.Aeson (decodeFileThrow)
import GHC.Generics
import GHC.TypeNats (type (+), type (^))
import Options.Applicative
import System.IO.Unsafe
import ZkFold.Algebra.Class
import ZkFold.Data.Binary (fromByteString)
import ZkFold.Protocol.NonInteractiveProof (TrustedSetup)
import ZkFold.Protocol.NonInteractiveProof.TrustedSetup (powersOfTauSubset)
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Prover.API.Server
import ZkFold.Prover.API.Types.ProveAlgorithm (ProveAlgorithm (proveAlgorithm))
import ZkFold.Symbolic.Examples.SmartWallet (
  ExpModProofInput,
  ZKF (..),
  ZKProofBytes,
  expModCircuit,
  expModProof,
  mkProof,
 )
import Prelude hiding (Bool, (==))
import ZkFold.Prover.API.Utils (addSwaggerDescription)

configPathParser ∷ Parser FilePath
configPathParser =
  option
    str
    ( long "config"
        <> help "Path to server configuration yaml file"
        <> showDefault
        <> value "./smart-wallet-prover-config.yaml"
        <> metavar "PATH"
    )

instance Swagger.ToSchema ZKF where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Field element."

instance ToSchema ExpModProofInput

instance Swagger.ToSchema ZKProofBytes where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Proof bytes where bytes are represented in hexadecimal encoding."


ts ∷ TrustedSetup (2 ^ 18 + 6)
{-# NOINLINE ts #-}
ts = unsafePerformIO powersOfTauSubset

instance ProveAlgorithm ExpModProofInput ZKProofBytes where
  proveAlgorithm zkProofInput = proofBytes
   where
    randomFieldElement = fromMaybe zero . fromByteString <$> Crypto.getRandomBytes 32
    proverSecret = PlonkupProverSecret <$> sequence (tabulate $ const randomFieldElement)
    !proofBytes = mkProof $ expModProof @ByteString ts (unsafePerformIO proverSecret) expModCircuit zkProofInput

deriving instance Generic ServerConfig

instance FromJSON ServerConfig

main ∷ IO ()
main = do
  serverConfigPath ← execParser opts
  print serverConfigPath
  serverConfig ← decodeFileThrow serverConfigPath
  print @String ("Started with " <> show serverConfig)
  runServer @ExpModProofInput @ZKProofBytes serverConfig
 where
  opts =
    info
      (configPathParser <**> helper)
      ( fullDesc
          <> progDesc "Smart Wallet prover"
          <> header "zkFold's Smart Wallet prover server"
      )
