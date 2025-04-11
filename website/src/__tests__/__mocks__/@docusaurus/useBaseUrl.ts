/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Mock implementation of useBaseUrl for testing
const useBaseUrl = (path: string): string => {
    // Ensure the path starts with a slash
    return path.startsWith('/') ? path : `/${path}`;
};

export default useBaseUrl;
