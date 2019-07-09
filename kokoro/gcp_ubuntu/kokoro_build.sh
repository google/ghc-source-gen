#!/bin/bash

set -xeuo pipefail

cd "${KOKORO_ARTIFACTS_DIR}"/github/ghc-source-gen

STACK_URL=https://github.com/commercialhaskell/stack/releases/download/v2.1.1/stack-2.1.1-linux-x86_64.tar.gz

mkdir -p "$HOME"/.local/bin
export PATH="$HOME/.local/bin:$PATH"

curl -L "$STACK_URL" | tar xz --wildcards --strip-components=1 -C $HOME/.local/bin '*/stack'

./check.sh
