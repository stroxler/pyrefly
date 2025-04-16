#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This script builds the WASM files specifically for testing with NodeJS target.
# It is meant to be run before Jest tests to ensure the WASM files are built with the correct target.
#
# This script:
# 1) Runs the wasm build script in the pyrefly_wasm folder with the --BUILD_FOR_TEST flag
# 2) Copies the generated WASM files to the website folder for testing
#

# Fail if we have any errors
set -e
set -o pipefail

# Change to `pyrefly` directory
cd -- "$(dirname -- "$0")/.."

../pyrefly_wasm/build.sh --BUILD_FOR_TEST || true
echo "copying wasm files from pyrefly_wasm/ to website/ for testing"
mkdir -p src/__tests__/wasm || true
cp ../pyrefly_wasm/target/pyrefly_wasm.js src/__tests__/wasm/pyrefly_wasm_for_testing.js || true
cp ../pyrefly_wasm/target/pyrefly_wasm_bg.wasm.opt src/__tests__/wasm/pyrefly_wasm_bg.wasm || true
echo "Finished copying WASM files for testing"
