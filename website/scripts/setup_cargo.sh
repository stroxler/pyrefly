#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Run the cargo setup script if it is available

# Fail if we have any errors
set -e

CARGO_SCRIPT=../facebook/setup_cargo.sh

if [ -f $CARGO_SCRIPT ]; then
    # shellcheck source=/dev/null
    source $CARGO_SCRIPT
fi
