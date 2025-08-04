module ZkFold.Cardano.SmartWallet.Api.Prove (
  prove,
  getProofStatus,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar qualified as STM
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM qualified as STM
import Crypto.Random.Types qualified as Crypto
import Data.ByteString (ByteString)
import Data.Functor.Rep (tabulate)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import ZkFold.Algebra.Class
import ZkFold.Data.Binary (fromByteString)
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Symbolic.Examples.SmartWallet (expModCircuit, expModProof, mkProof)

import ZkFold.Cardano.SmartWallet.Api.Encryption
import ZkFold.Cardano.SmartWallet.Types.Common
import ZkFold.Cardano.SmartWallet.Types.Encryption
import ZkFold.Cardano.SmartWallet.Types.Errors
import ZkFold.Cardano.SmartWallet.Types.Prove

-- | Initialise a ZK Proof and obtain a proof identifier.
-- The proof input must be encrypted with one of the server's public keys.
prove ∷ ProveRequestMonad m ⇒ Proofs → STM.TVar [KeyPair] → ZKProveRequest → m ProofId
prove proofs keys request = do
  pid ← randomProofId
  zkProofInput ← decryptInput keys request
  liftIO . STM.atomically $ do
    m ← STM.readTVar proofs
    STM.writeTVar proofs $ M.insert pid Nothing m
  _ ← liftIO . forkIO $ do
    let randomFieldElement = fromMaybe zero . fromByteString <$> Crypto.getRandomBytes 32
    proverSecret ← PlonkupProverSecret <$> sequence (tabulate $ const randomFieldElement)
    let !proofBytes = mkProof $ expModProof @ByteString one proverSecret expModCircuit zkProofInput
    time ← getCurrentTime
    STM.atomically $ do
      m ← STM.readTVar proofs
      let result = ZKProveResult proofBytes time
      STM.writeTVar proofs $ M.insert pid (Just result) m
  pure pid

-- | Query whether the proof has finished using its identifier
getProofStatus ∷ ProveRequestMonad m ⇒ Proofs → ProofId → m ProofStatus
getProofStatus proofs pid = do
  m ← liftIO $ STM.atomically $ STM.readTVar proofs
  case M.lookup pid m of
    Nothing → throw $ ZKPEProofError (ZKInvalidProofID $ proofIdToText pid)
    Just Nothing → pure Pending
    Just (Just bytes) → pure $ Completed bytes
