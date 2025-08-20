# syntax=docker/dockerfile:1.7-labs

# To build this image, run:
# docker build --tag wenkokke/hs-onnxruntime-capi:0.1.0.0 --file Dockerfile .

FROM ubuntu:24.04 as build

# Configure versions
ARG GHC_VERSION="9.6.7"
ARG CABAL_VERSION="3.12.1.0"
ARG ONNXRUNTIME_VERSION="1.22.2"

# Install system dependencies
RUN <<EOF
apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
        apt-get install -y \
            build-essential \
            cmake \
            curl \
            git \
            libffi-dev \
            libffi8 \
            libgmp-dev \
            libgmp10 \
            libncurses-dev \
            libncurses6 \
            libtinfo6 \
            python3
EOF

# Install GHCUp
RUN <<EOF
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
EOF
ENV PATH="${PATH}:/root/.ghcup/bin:/root/.cabal/bin"

# Install GHC and Cabal
RUN <<EOF
ghcup install ghc   ${GHC_VERSION}
ghcup set     ghc   ${GHC_VERSION}
ghcup install cabal ${CABAL_VERSION}
ghcup set     cabal ${CABAL_VERSION}
EOF

# Get ONNX Runtime sources
RUN <<EOF
git clone \
    --depth 1 \
    --branch v${ONNXRUNTIME_VERSION} \
    --recursive \
    https://github.com/Microsoft/onnxruntime.git \
    /onnxruntime
EOF

# Enter ONNX Runtime source directory
WORKDIR "/onnxruntime"

# Build ONNX Runtime
# NOTE: test QDQTransformerTests.MatMul_U8U8S8_FastMath fails
RUN <<EOF
./build.sh \
    --config RelWithDebInfo \
    --build_shared_lib \
    --parallel \
    --compile_no_warning_as_error \
    --allow_running_as_root || \
    true
EOF

# Add ONNX Runtime to LD_LIBRARY_PATH
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/onnxruntime/build/Linux/RelWithDebInfo"

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
    extra-include-dirs: /onnxruntime/include/onnxruntime/core/session/
    extra-lib-dirs: /onnxruntime/build/Linux/RelWithDebInfo
    flags: -pkg-config -use-bundled-header
EOF

# Build hs-onnxruntime-capi
RUN cabal v2-build all

# Test hs-onnxruntime-capi
RUN cabal v2-test all
