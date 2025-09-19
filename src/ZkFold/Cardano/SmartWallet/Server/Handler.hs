module ZkFold.Cardano.SmartWallet.Server.Handler (
  ProverAPI,
  proverApi,
  handleProverApi,
  MainAPI,
  mainAPI,
  handleMainApi,
) where

import Control.Lens ((.~), (?~))
import Data.Function ((&))
import Data.Proxy
import Data.Swagger
import Data.Text qualified as T
import Data.Version (showVersion)
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import PackageInfo_smart_wallet_prover qualified as PackageInfo
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

proverSwagger ∷ Swagger
proverSwagger =
  toSwagger proverApi
    & info
      . title
      .~ "zkFold Smart Wallet Prover API"
    & info
      . version
      .~ (T.pack . showVersion $ PackageInfo.version)
    & info
      . license
      ?~ ("Apache-2.0" & url ?~ URL "https://opensource.org/licenses/apache-2-0")
    & info
      . contact
      ?~ ( mempty
             & url
               ?~ URL "https://zkfold.io/"
             & email
               ?~ "info@zkfold.io"
             & name
               ?~ "zkFold Technical Support"
         )
    & info
      . description
      ?~ "ZK Prover."
    & applyTagsFor
      (subOperations (Proxy ∷ Proxy ProverAPI) (Proxy ∷ Proxy ProverAPI))
      ["ZK prover endpoints" & description ?~ "Get server public keys, submit a proof and get proof status."]

handleProverApi ∷ Ctx → Server ProverAPI
handleProverApi ctx =
  handleGetKeys ctx
    :<|> handleProve ctx
    :<|> handleProofStatus ctx

handleGetKeys ∷ Ctx → Handler [PublicKeyBundle]
handleGetKeys Ctx {..} = getPublicKeys ctxServerKeys

handleProve ∷ Ctx → ZKProveRequest → Handler ProofId
handleProve Ctx {..} = prove ctxTrustedSetup ctxProofsDatabase ctxServerKeys

handleProofStatus ∷ Ctx → ProofId → Handler ProofStatus
handleProofStatus Ctx {..} = getProofStatus ctxProofsDatabase

type MainAPI = ProverAPI :<|> SwaggerSchemaUI "docs" "swagger.json"

mainAPI ∷ Proxy MainAPI
mainAPI = Proxy

handleMainApi ∷ Ctx → Server MainAPI
handleMainApi ctx = handleProverApi ctx :<|> swaggerSchemaUIServer proverSwagger
