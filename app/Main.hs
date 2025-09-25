{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Crypto.Random.Types qualified as Crypto
import Data.ByteString (ByteString)
import Data.Functor.Rep (tabulate)
import Data.Maybe (fromMaybe)
import Data.OpenApi (NamedSchema (..), ToSchema (..))
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
  ByteStringFromHex,
  ExpModProofInput,
  ZKF,
  ZKProofBytes,
  expModCircuit,
  expModProof,
  mkProof,
 )
import Prelude hiding (Bool, (==))

portParser ∷ Parser Int
portParser =
  option
    auto
    ( long "port"
        <> help "Port to listen for proof requests"
        <> showDefault
        <> value 8083
        <> metavar "INT"
    )

deriving newtype instance ToSchema ZKF

instance ToSchema ExpModProofInput

instance ToSchema ByteStringFromHex where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "Byte string in hex encoding") mempty

instance ToSchema ZKProofBytes

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
  serverPort ← execParser opts
  let
    dbHost = "localhost"
    dbName = "postgres"
    dbUser = "postgres"
    dbPassword = "password"
    dbPort = 5432
    nWorkers = 3
    contractId = 1

  let serverConfig = ServerConfig {..}
  print @String ("Started with " <> show serverConfig)
  runServer @ExpModProofInput @ZKProofBytes serverConfig
 where
  opts =
    info
      (portParser <**> helper)
      ( fullDesc
          <> progDesc "Smart Wallet prover"
          <> header "zkFold's Smart Wallet prover server"
      )
