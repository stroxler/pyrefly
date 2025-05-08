/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

/**
 * Enum representing the different states of Pyodide execution
 */
export enum PyodideStatus {
    /**
     * Pyodide has not been initialized
     */
    NOT_INITIALIZED = 'not_initialized',

    /**
     * Pyodide is currently initializing
     */
    INITIALIZING = 'initializing',

    /**
     * Pyodide is currently running Python code
     */
    RUNNING = 'running',

    /**
     * Pyodide has finished running Python code
     */
    FINISHED_RUNNING = 'finished_running',
}
