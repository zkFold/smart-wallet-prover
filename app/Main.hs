{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Crypto.Random.Types qualified as Crypto
import Data.ByteString (ByteString)
import Data.Functor.Rep (tabulate)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Data.Swagger as Swagger hiding (info)
import GHC.TypeNats (type (+), type (^))
import Options.Applicative
import System.IO.Unsafe
import ZkFold.Algebra.Class
import ZkFold.Data.Binary (fromByteString)
import ZkFold.Protocol.NonInteractiveProof.TrustedSetup (TrustedSetup, powersOfTauSubset)
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Prover.API.Server
import ZkFold.Prover.API.Types.ProveAlgorithm (ProveAlgorithm (proveAlgorithm))
import ZkFold.Prover.API.Utils (addSwaggerDescription)
import ZkFold.Symbolic.Examples.SmartWallet (
  ExpModProofInput,
  ZKF (..),
  ZKProofBytes,
  expModCircuit,
  expModProof,
  mkProof,
 )
import Prelude hiding (Bool, (==))
import ZkFold.Prover.API.Types.Config

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

main ∷ IO ()
main = do
  serverConfig ← execParser opts
  print @String ("Started with " <> show serverConfig)
  runServer @ExpModProofInput @ZKProofBytes serverConfig
 where
  opts =
    info
      (cliParser defaultServerConfig <**> helper)
      ( fullDesc
          <> progDesc "Smart Wallet prover"
          <> header "zkFold's Smart Wallet prover server"
      )
