FROM ubuntu:22.04

WORKDIR /app

# Copy all files
COPY . /app/

# Install dependencies
RUN apt-get update -y && \
    apt-get install -y \ 
    bash curl git make gcc g++ \ 
    libgmp-dev \
    liblzma-dev \
    zlib1g-dev \
    pkg-config \
    build-essential \
    curl

# Install ghcup, ghc and cabal
RUN curl -L https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup -o /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"

RUN ghcup install cabal 3.14.2.0 && \
    ghcup install ghc 9.12.2 && \
    ghcup set 9.12.2

# Install rustup and cargo
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y

ENV PATH="/root/.cargo/bin:${PATH}"

# Build server
RUN cabal update

RUN cabal build

CMD ["cabal", "run", "smart-wallet-prover"]