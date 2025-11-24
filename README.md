# zkFold Smart Wallet Prover server 

This repository houses the server for generating zero-knowledge proofs for ZK based smart wallet by [zkFold](https://zkfold.io/). 

## Configuration
Smart Wallet Prover server is defined as a `symbolic-prover-api` instance. You can see how to configure the server at startup [here](https://github.com/zkFold/symbolic-prover-api?tab=readme-ov-file#symbolic-prover-api)

For example:
```bash
cabal run smart-wallet-prover -- 
    \ --port 8083 
    \ --db-file sqlite.db
    \ --mode encrypted
    \ --n-workers 2
    \ --proof-lifetime 30
    \ --keys-lifetime 86400
    \ --config config.yaml
```

## Docker

### Required dependencies
You need [docker](https://docs.docker.com/engine/install/) for run Smart Wallet Prover server


### Start server

For first running you need to build docker image:
```bash
docker build -t smart-wallet-prover:latest .
```

After that, you will be able to run the Docker container and the server inside it simply by using next command. Specify the ports you need instead of 8083
```bash
docker run -p 8083:8083 smart-wallet-prover:latest
```

## Example query
You can send a request to get keys by specifying the port and host
```bash
curl -X 'GET' localhost:8083/v0/keys
```

## API documentation

Endpoints made available by server are specified [here](https://wallet-prover.zkfold.io/docs).
