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
import Data.Aeson.Casing
import Data.Char (isLower)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Proxy
import Data.Swagger qualified as Swagger
import Data.Swagger.Declare qualified as Swagger
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import Deriving.Aeson
import ZkFold.Symbolic.Examples.SmartWallet (ByteStringFromHex (..), ZKProofBytes (..))

import ZkFold.Cardano.SmartWallet.Orphans ()
import ZkFold.Cardano.SmartWallet.Types.Common (addDescription, addFieldDescription)
import ZkFold.Cardano.SmartWallet.Types.Encryption (KeyID)
import ZkFold.Cardano.SmartWallet.Utils

data ZKProveRequest
  = ZKProveRequest
  { preqServerKeyId ∷ KeyID
  , preqAesEncryptionKey ∷ ByteStringFromHex
  , preqEncryptedPayload ∷ ByteStringFromHex
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "preq", CamelToSnake]] ZKProveRequest

instance Swagger.ToSchema ZKProveRequest where
  declareNamedSchema _ = do
    schema ←
      Swagger.genericDeclareNamedSchema
        Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = snakeCase . dropWhile isLower}
        (Proxy ∷ Proxy ZKProveRequest)
    defs ← Swagger.look
    let inlineSchema@Swagger.NamedSchema {..} = Swagger.inlineNonRecursiveSchemas defs schema
    pure inlineSchema {Swagger._namedSchemaSchema = customise _namedSchemaSchema}
   where
    encryptionInstructions =
      T.unlines
        [ "Data required for ZK proof"
        , "The unencrypted payload must be a JSON of the following format:"
        , "\t"
        , "\t{                        "
        , "\t\tpiPubE: integer,       "
        , "\t\tpiPubN: integer,       "
        , "\t\tpiSignature: integer,  "
        , "\t\tpiTokenName: integer   "
        , "\t}                        "
        , "\t"
        , "piPubE is the public exponent used in the RSA key used to sign the JSON Web Token, as a decimal integer."
        , "piPubN is the public modulus used in the RSA key used to sign the JSON Web Token, as a decimal integer."
        , "piSignature is the signature attached to the JSON Web Token, as a decimal integer."
        , "piTokenName is the hash of the public key corresponding to the Wallet's root key, as a decimal integer."
        , "⋅"
        , "The Prover server uses combined encryption to secure sensitive information."
        , "Payload is encrypted with a symmetric cipher, while its encryption key is itself encrypted with the Server's 2048-bit RSA key."
        , "⋅"
        , "The JSON from above must be encrypted with AES-256-CBC and PKCS#7 padding, making the \"encrypted_payload\" field."
        , "AES-256 encryption key must itself be encrypted with one of the Prover server's public keys, making the \"aes_encryption_key\" field."
        , "The key ID of the Server's RSA key used to encrypt the AES key makes the \"server_key_id\" field."
        , "⋅"
        ]
    customise schema =
      schema
        & addDescription encryptionInstructions
        & addFieldDescription "server_key_id" "ZK Prover server public key ID used to encrypt the AES key."
        & addFieldDescription
          "aes_encryption_key"
          "Hex-encoded AES-256 key for decyphering the payload. The key should be encrypted with one of the server's RSA keys."
        & addFieldDescription "encrypted_payload" "Hex-encoded ZK Prover input encrypted with AES-256-CBC and PKCS#7 padding."

-- | Proof bytes with their creation time.
-- It might be useful if we decide to remove old results.
data ZKProveResult
  = ZKProveResult
  { presBytes ∷ ZKProofBytes
  , presTimestamp ∷ UTCTime
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "pres", CamelToSnake]] ZKProveResult

instance Swagger.ToSchema ZKProveResult where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema
      Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = snakeCase . dropWhile isLower}
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
