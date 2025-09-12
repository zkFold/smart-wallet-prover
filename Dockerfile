# FROM rust:latest
FROM haskell:9.12.2

WORKDIR /app

COPY smart-wallet-prover.cabal cabal.project /app/

COPY . /app/

RUN apt-get update -y && \
apt-get install -y \ 
pkg-config \
build-essential \
curl

RUN apt install liblzma-dev

RUN curl https://sh.rustup.rs -sSf | bash -s -- -y

ENV PATH="/root/.cargo/bin:${PATH}"

RUN cabal update

RUN cabal install --dependencies-only

RUN cabal build

CMD ["cabal", "run", "smart-wallet-prover"]