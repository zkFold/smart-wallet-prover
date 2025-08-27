module ZkFold.Cardano.SmartWallet.Server.Handler (
  ProverAPI,
  proverApi,
  handleProverApi,
  MainAPI,
  mainAPI,
  handleMainApi,
) where

import Data.Proxy
import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI

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

swagger ∷ Swagger
swagger = toSwagger proverApi

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

type MainAPI = ProverAPI :<|> SwaggerSchemaUI "docs" "swagger.json"

mainAPI ∷ Proxy MainAPI
mainAPI = Proxy

handleMainApi ∷ Ctx → Server MainAPI
handleMainApi ctx = handleProverApi ctx :<|> swaggerSchemaUIServer swagger
