/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

// Mock implementation of usePythonWorker hook
export const usePythonWorker = jest.fn(({ setIsRunning, setPythonOutput }) => {
    // Return a mock runPython function that does nothing
    const runPython = jest.fn(async (code: string): Promise<void> => {
        // Do nothing when called
        return Promise.resolve();
    });

    return { runPython };
});
