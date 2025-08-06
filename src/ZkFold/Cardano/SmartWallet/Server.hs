module ZkFold.Cardano.SmartWallet.Server (
  runServer,
) where

import Control.Concurrent.STM (newTVarIO)
import Data.Map.Strict qualified as M
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, corsRequestHeaders)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant

import ZkFold.Cardano.SmartWallet.Server.Handler
import ZkFold.Cardano.SmartWallet.Types

-- Allow all origins and common methods/headers
simpleCorsResourcePolicy ∷ CorsResourcePolicy
simpleCorsResourcePolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing -- Nothing means allow all origins
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = ["Content-Type"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Just 3600
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

corsMiddleware ∷ Middleware
corsMiddleware = cors (const $ Just simpleCorsResourcePolicy)

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
  run port $ logStdout $ corsMiddleware $ serve proverApi $ handleProverApi ctx
