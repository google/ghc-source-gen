#!/bin/bash
# Copyright 2019 Google LLC
#
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file or at
# https://developers.google.com/open-source/licenses/bsd

# This script tests that ghc-source-gen works on multiple GHC versions.
# TODO: turn this into a CI script.

set -ueo pipefail

for flag in --resolver={lts-11.22,lts-12.8,lts-13.23} --stack-yaml=stack-8.8.yaml
do
    echo ====== $flag ======
    stack test --no-terminal $flag
done
