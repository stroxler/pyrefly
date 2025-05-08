/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import { PyodideStatus } from './PyodideStatus';

/*
 * This file is a Web Worker that runs Python code using Pyodide. It is used by the
 * Pyrefly sandbox to run Python code in the browser. We use a web worker to run
 * Python code in a separate thread, so that the main thread can continue to
 * render the UI while the Python code is running. This is especially useful in cases
 * where the Python code takes a long time to run.
 *
 */
import { loadPyodide, PyodideInterface } from 'pyodide';
export type WorkerResponse =
    | stdoutResponseOrStderrResponse
    | RunPythonResponse
    | PyodideStatusResponse;

type stdoutResponseOrStderrResponse = {
    type: 'stdout' | 'stderr';
    output: string;
};

type RunPythonResponse = {
    type: 'runPython';
    success: boolean;
    error?: string;
};

type PyodideStatusResponse = {
    type: 'pyodideStatusUpdate';
    status: PyodideStatus;
};

let pyodideInstance: PyodideInterface = null;

const initPyodide = async () => {
    // Notify the main thread that we're initializing Pyodide
    self.postMessage({
        type: 'pyodideStatusUpdate',
        status: PyodideStatus.INITIALIZING,
    });

    const py = await loadPyodide({
        indexURL: `${self.location.origin}/pyodide`,
    });

    // Notify the main thread that Pyodide has been initialized
    self.postMessage({
        type: 'pyodideStatusUpdate',
        status: PyodideStatus.RUNNING,
    });

    // Set up stdout and stderr capture
    py.setStdout({
        batched: (output: string) => {
            self.postMessage({
                type: 'stdout',
                output,
            });
        },
    });

    py.setStderr({
        batched: (output: string) => {
            self.postMessage({
                type: 'stderr',
                output,
            });
        },
    });

    return py;
};

// Handle messages from the main thread
self.onmessage = async (event: MessageEvent<string>) => {
    const code = event.data;

    try {
        if (!pyodideInstance) {
            pyodideInstance = await initPyodide();
        }
        pyodideInstance.runPython(code);

        // Send successful response back to main thread
        const response: WorkerResponse = {
            type: 'runPython',
            success: true,
        };
        self.postMessage(response);
    } catch (error) {
        console.error('Python error:', error);

        // Send error response back to main thread
        const response: WorkerResponse = {
            type: 'runPython',
            success: false,
            error: error instanceof Error ? error.message : 'Unknown error',
        };
        self.postMessage(response);
    }
};
