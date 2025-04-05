/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Mock implementation of lz-string module for testing
const lzString = {
    compressToEncodedURIComponent: jest.fn().mockReturnValue('compressed'),
    decompressFromEncodedURIComponent: jest.fn().mockReturnValue('from typing import *')
};

module.exports = lzString;
