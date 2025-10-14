# zkFold Smart Wallet Prover server 

This repository houses the server for generating zero-knowledge proofs for ZK based smart wallet by [zkFold](https://zkfold.io/). 

## Required dependencies
You need [docker](https://docs.docker.com/engine/install/) and [docker compose](https://docs.docker.com/compose/install/) for run Smart Wallet Prover server

## Start server

For first running you need to build docker image:
```bash
docker compose up --build server
```

After that, you will be able to run the Docker container and the server inside it simply by using:
```bash
docker compose up server
```

## Example query
You can send a request to get keys by specifying the port and host
```bash
curl -X 'GET' localhost:8083/v0/keys
```

## API documentation

Endpoints made available by server are specified [here](https://wallet-prover.zkfold.io/docs).
