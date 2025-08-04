module ZkFold.Cardano.SmartWallet.Types.Prove (
  ZKProveRequest (..),
  ZKProveResult (..),
  ProofStatus (..),
  ProofId,
  randomProofId,
  proofIdToText,
  Proofs,
) where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Swagger qualified as Swagger
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import Deriving.Aeson
import ZkFold.Symbolic.Examples.SmartWallet (ByteStringFromHex (..), ZKProofBytes (..))

import ZkFold.Cardano.SmartWallet.Orphans ()
import ZkFold.Cardano.SmartWallet.Types.Encryption (KeyID)
import ZkFold.Cardano.SmartWallet.Utils

data ZKProveRequest
  = ZKProveRequest
  { preqKeyId ∷ KeyID
  , preqAES ∷ ByteStringFromHex
  , preqPayload ∷ ByteStringFromHex
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Swagger.ToSchema ZKProveRequest where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Data required for ZK proof"

-- | Proof bytes with their creation time.
-- It might be useful if we decide to remove old results.
data ZKProveResult
  = ZKProveResult
  { presBytes ∷ ZKProofBytes
  , presTimestamp ∷ UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Swagger.ToSchema ZKProveResult where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "ZK proof bytes with a timestamp"

data ProofStatus = Pending | Completed ZKProveResult
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Swagger.ToSchema ProofStatus where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchemaUnrestricted Swagger.defaultSchemaOptions
      & addSwaggerDescription "Status of a submitted proof"

newtype ProofId = ProofId UUID
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

instance Swagger.ToSchema ProofId where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "ID of a submitted prove request"

randomProofId ∷ MonadIO m ⇒ m ProofId
randomProofId = ProofId <$> liftIO nextRandom

proofIdToText ∷ ProofId → Text
proofIdToText (ProofId pid) = toText pid

-- | A shared Map with data about proofs accessible by their IDs.
-- If an ID isn't in the Map, there was no proof with such ID registered.
-- If the value associated with a given ID is Nothing, the proof hasn't finished yet.
-- Otherwise, the Map will contain Proof bytes.
type Proofs = TVar (Map ProofId (Maybe ZKProveResult))
