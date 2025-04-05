/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as React from 'react';

// Mock implementation of BrowserOnly for testing
// Renders its children directly without checking if in browser environment

/**
 * Props for BrowserOnly component
 */
interface BrowserOnlyProps {
    children: () => React.ReactNode;
}

/**
 * Mock implementation of BrowserOnly component
 * @param props Component props
 * @returns The result of calling the children function
 */
const BrowserOnly = ({ children }: BrowserOnlyProps): React.ReactNode => children();

module.exports = BrowserOnly;
