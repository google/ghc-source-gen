#!/bin/bash

set -xueo pipefail
mkdir -p $HOME/.local/bin
curl -L https://github.com/commercialhaskell/stack/releases/download/v2.1.1/stack-2.1.1-linux-x86_64.tar.gz \
    | tar xz --wildcards --strip-components=1 -C "$HOME/.local/bin" '*/stack'
echo 'export PATH=$HOME/.local/bin:$PATH' >> $BASH_ENV


