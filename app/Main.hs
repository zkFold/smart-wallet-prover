import Options.Applicative

import ZkFold.Cardano.SmartWallet.Server

newtype Port = Port Int

portParser ∷ Parser Port
portParser =
  Port
    <$> option
      auto
      ( long "port"
          <> help "Port to listen for proof requests"
          <> showDefault
          <> value 8083
          <> metavar "INT"
      )

main ∷ IO ()
main = do
  Port port ← execParser opts
  runServer port
 where
  opts =
    info
      (portParser <**> helper)
      ( fullDesc
          <> progDesc "Smart Wallet prover"
          <> header "zkFold's Smart Wallet prover server"
      )
