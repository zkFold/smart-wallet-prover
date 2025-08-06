module ZkFold.Cardano.SmartWallet.Server.Handler (
  ProverAPI,
  proverApi,
  handleProverApi,
) where

import Data.Proxy
import Servant

import ZkFold.Cardano.SmartWallet.Api
import ZkFold.Cardano.SmartWallet.Types

type ProverEndpoints =
  Summary "Get server public keys."
    :> "keys"
    :> Get '[JSON] [PublicKeyBundle]
    :<|> Summary "Submit data for proving."
      :> "prove"
      :> ReqBody '[JSON] ZKProveRequest
      :> Post '[JSON] ProofId
    :<|> Summary "Check the status of a proof."
      :> "proof-status"
      :> ReqBody '[JSON] ProofId
      :> Post '[JSON] ProofStatus

type ProverAPI = "v0" :> ProverEndpoints

proverApi ∷ Proxy ProverAPI
proverApi = Proxy ∷ Proxy ProverAPI

handleProverApi ∷ Ctx → Server ProverAPI
handleProverApi ctx =
  handleGetKeys ctx
    :<|> handleProve ctx
    :<|> handleProofStatus ctx

handleGetKeys ∷ Ctx → Handler [PublicKeyBundle]
handleGetKeys Ctx {..} = getPublicKeys ctxServerKeys

handleProve ∷ Ctx → ZKProveRequest → Handler ProofId
handleProve Ctx {..} = prove ctxProofsDatabase ctxServerKeys

handleProofStatus ∷ Ctx → ProofId → Handler ProofStatus
handleProofStatus Ctx {..} = getProofStatus ctxProofsDatabase
