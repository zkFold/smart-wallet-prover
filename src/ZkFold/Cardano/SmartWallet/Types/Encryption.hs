module ZkFold.Cardano.SmartWallet.Types.Encryption (
  KeyID,
  keyIdToText,
  randomKeyID,
  PublicKey,
  PublicKeyBundle (..),
  PrivateKey,
  KeyPair (..),
  randomKeyPair,
  removePrivateKey,
  decrypt,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Crypto.PubKey.RSA (generate)
import Crypto.PubKey.RSA qualified as RSA
import Crypto.PubKey.RSA.PKCS15 qualified as PKCS15
import Crypto.Random.Types qualified as Crypto
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Swagger qualified as Swagger
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Deriving.Aeson

import ZkFold.Cardano.SmartWallet.Orphans ()
import ZkFold.Cardano.SmartWallet.Types.Common
import ZkFold.Cardano.SmartWallet.Types.Errors
import ZkFold.Cardano.SmartWallet.Utils

newtype KeyID = KeyID UUID
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

instance Swagger.ToSchema KeyID where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Encrypting key ID"

keyIdToText ∷ KeyID → Text
keyIdToText = UUID.toText . coerce

randomKeyID ∷ MonadIO m ⇒ m KeyID
randomKeyID = liftIO $ coerce <$> UUID.nextRandom

newtype PublicKey = PublicKey RSA.PublicKey
  deriving stock (Eq, Generic, Show)

instance FromJSON PublicKey where
  parseJSON = withObject "PublicKey" $ \v →
    fmap PublicKey $
      RSA.PublicKey
        <$> fmap read (v .: "public_size")
        <*> fmap read (v .: "public_n")
        <*> fmap read (v .: "public_e")

instance ToJSON PublicKey where
  toJSON (PublicKey pkey) =
    object
      [ "public_size" .= show (RSA.public_size pkey)
      , "public_n" .= show (RSA.public_n pkey)
      , "public_e" .= show (RSA.public_e pkey)
      ]

instance Swagger.ToSchema PublicKey where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Public key for encrypting ZK proof"

newtype PrivateKey = PrivateKey RSA.PrivateKey
  deriving stock (Eq, Generic, Show)

data KeyPair
  = KeyPair
  { kpId ∷ KeyID
  , kpPublic ∷ PublicKey
  , kpPrivate ∷ PrivateKey
  , kpExpires ∷ UTCTime
  }
  deriving stock (Eq, Generic, Show)

randomKeyPair ∷ (MonadIO m, Crypto.MonadRandom m) ⇒ m KeyPair
randomKeyPair = do
  time ← liftIO getCurrentTime
  let expires = addUTCTime nominalDay time -- expires in a day -- just for testing purposes
  (pub, priv) ← generate 2048 65537
  kid ← randomKeyID
  pure $ KeyPair kid (PublicKey pub) (PrivateKey priv) expires

data PublicKeyBundle
  = PublicKeyBundle
  { pkbId ∷ KeyID
  , pkbPublic ∷ PublicKey
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Swagger.ToSchema PublicKeyBundle where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Public key with its ID"

removePrivateKey ∷ KeyPair → PublicKeyBundle
removePrivateKey KeyPair {..} = PublicKeyBundle kpId kpPublic

decrypt ∷ ProveRequestMonad m ⇒ Maybe Text → PrivateKey → ByteString → m ByteString
decrypt maybeName (PrivateKey pkey) bs = do
  let errorMsg = "Could not decrypt the " <> fromMaybe "byte string" maybeName
  decrypted ← PKCS15.decryptSafer pkey bs
  case decrypted of
    Left _ → throw (ZKPEDecryptionError $ ZKDecryptionFailed errorMsg)
    Right res → pure res
