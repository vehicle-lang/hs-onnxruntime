# syntax=docker/dockerfile:1.7-labs

# To build this image, run:
# docker build --tag wenkokke/hs-onnxruntime-capi:0.1.0.0 --file Dockerfile .

FROM debian:bookworm-slim AS build-onnxruntime

# Configure versions
ARG ONNXRUNTIME_VERSION="1.22.2"
ARG CMAKE_VERSION="4.1.0"

# Install system dependencies
RUN <<EOF
apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
        apt-get install -y \
            build-essential \
            curl \
            git \
            libssl-dev \
            python3
EOF

# Get CMake sources
RUN curl -L https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-${CMAKE_VERSION}.tar.gz | tar xzv -C /tmp

# Enter CMake source directory
WORKDIR "/tmp/cmake-${CMAKE_VERSION}"

# Install CMake
RUN ./bootstrap && gmake && gmake install

# Get ONNX Runtime sources
RUN <<EOF
git clone \
    --depth 1 \
    --shallow-submodules \
    --branch v${ONNXRUNTIME_VERSION} \
    --recursive \
    https://github.com/Microsoft/onnxruntime.git \
    /tmp/onnxruntime
EOF

# Enter ONNX Runtime source directory
WORKDIR "/tmp/onnxruntime"

# Build ONNX Runtime
RUN <<EOF
./build.sh \
    --allow_running_as_root \
    --build_shared_lib \
    --compile_no_warning_as_error \
    --config RelWithDebInfo \
    --parallel \
    --skip_submodule_sync \
    --skip_tests
EOF

FROM debian:bookworm-slim AS build

# Copy ONNX Runtime
COPY --from=build-onnxruntime "/tmp/onnxruntime/include" "/onnxruntime/include"
COPY --from=build-onnxruntime "/tmp/onnxruntime/build/Linux/RelWithDebInfo" "/onnxruntime/lib"

# Add ONNX Runtime to LD_LIBRARY_PATH
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/onnxruntime/lib"

ARG GHC_VERSION="9.6.7"
ARG CABAL_VERSION="3.12.1.0"

# Install system dependencies
RUN <<EOF
apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
        apt-get install -y \
            build-essential \
            curl \
            libffi-dev \
            libffi8 \
            libgmp-dev \
            libgmp10 \
            libncurses-dev \
            libncurses6 \
            libtinfo6
EOF

# Install GHCUp
RUN <<EOF
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
EOF
ENV PATH="${PATH}:/root/.ghcup/bin:/root/.cabal/bin"

# Install GHC and Cabal
RUN <<EOF
ghcup install ghc   --set ${GHC_VERSION}
ghcup install cabal --set ${CABAL_VERSION}
EOF

# Update Cabal package index
RUN cabal v2-update

# Copy source files
COPY --parents * "/hs-onnxruntime/"

# Enter hs-onnxruntime-capi source directory
WORKDIR "/hs-onnxruntime"

# Configure hs-onnxruntime-capi
COPY <<EOF /hs-onnxruntime/cabal.project.local
ignore-project: False
tests: True

package hs-onnxruntime-capi
    extra-include-dirs: /onnxruntime/include
    extra-lib-dirs: /onnxruntime/lib
    flags: -pkg-config -use-bundled-header
EOF

# Build hs-onnxruntime-capi
RUN cabal v2-build all

# Test hs-onnxruntime-capi
RUN cabal v2-test all
