module ZkFold.Cardano.SmartWallet.Types.Ctx (
  Ctx (..),
) where

import Control.Concurrent.STM (TVar)
import ZkFold.Algebra.Number
import ZkFold.Protocol.NonInteractiveProof (TrustedSetup)

import ZkFold.Cardano.SmartWallet.Types.Encryption
import ZkFold.Cardano.SmartWallet.Types.Prove

-- | Server context: configuration & shared state.
data Ctx = Ctx
  { ctxProofsDatabase ∷ !Proofs
  , ctxServerKeys ∷ !(TVar [KeyPair])
  , ctxTrustedSetup ∷ !(TrustedSetup (2 ^ 18 + 6))
  }
