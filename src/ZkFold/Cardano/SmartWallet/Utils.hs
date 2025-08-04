module ZkFold.Cardano.SmartWallet.Utils (
  addSwaggerDescription,
) where

import Control.Lens (mapped, (?~))
import Data.Swagger qualified as Swagger

-- | Utility function to add swagger description to a schema.
--
-- FIXME This is copypaste from https://github.com/geniusyield/atlas/blob/9a20624356d13baceffe53cf2afcbeec170d5867/src/GeniusYield/Swagger/Utils.hs#L24
-- Importing this single function from GeniusYield will result in this server depending on all Cardano libraries
addSwaggerDescription
  ∷ (Functor f1, Functor f2, Swagger.HasSchema b1 a, Swagger.HasDescription a (Maybe b2)) ⇒ b2 → f1 (f2 b1) → f1 (f2 b1)
addSwaggerDescription desc = mapped . mapped . Swagger.schema . Swagger.description ?~ desc
