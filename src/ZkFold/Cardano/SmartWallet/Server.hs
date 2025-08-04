module ZkFold.Cardano.SmartWallet.Server (
  runServer,
) where

import Control.Concurrent.STM (newTVarIO)
import Data.Map.Strict qualified as M
import Network.Wai.Handler.Warp (run)
import Servant

import ZkFold.Cardano.SmartWallet.Server.Handler
import ZkFold.Cardano.SmartWallet.Types

runServer ∷ Int → IO ()
runServer port = do
  proofsDb ← newTVarIO M.empty
  key ← randomKeyPair
  keysVar ← newTVarIO [key]
  let
    ctx =
      Ctx
        { ctxProofsDatabase = proofsDb
        , ctxServerKeys = keysVar
        }
  run port $ serve proverApi $ handleProverApi ctx
