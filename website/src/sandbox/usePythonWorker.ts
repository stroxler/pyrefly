/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import { useEffect } from 'react';
import { WorkerResponse } from './pythonWorker';

interface usePythonWorkerProps {
    setPythonOutput: React.Dispatch<React.SetStateAction<string>>;
}

// Singleton instance variables
let workerInstance: Worker | null = null;
let resolveInstance: ((value: void | PromiseLike<void>) => void) | null = null;
let setOutputFunction: React.Dispatch<React.SetStateAction<string>> | null =
    null;

// Singleton runPython function
const runPython = async (code: string): Promise<void> => {
    if (!workerInstance || !setOutputFunction) {
        console.error(
            'Python worker not initialized. Call initializePythonWorker first.'
        );
        return;
    }

    setOutputFunction('');

    return new Promise((resolve) => {
        // Store the resolve function for later use
        resolveInstance = resolve;

        // Send message to worker
        workerInstance?.postMessage(code);
    });
};

// Initialize the Python worker singleton
const initializePythonWorker = (
    setPythonOutput: React.Dispatch<React.SetStateAction<string>>
): void => {
    // Only initialize once
    if (workerInstance !== null) {
        return;
    }

    // Store the output function
    setOutputFunction = setPythonOutput;

    // Create worker instance
    const worker = new Worker(new URL('./pythonWorker.ts', import.meta.url), {
        type: 'module',
    });

    // Set up message handler
    worker.onmessage = (event: MessageEvent<WorkerResponse>) => {
        const response: WorkerResponse = event.data;

        switch (response.type) {
            case 'stdout':
                setPythonOutput((prev) => prev + '\n' + response.output);
                return;
            case 'stderr':
                setPythonOutput((prev) => prev + '\n' + response.output);
                return;
            case 'runPython':
                if (!response.success && response.error) {
                    setPythonOutput(
                        (prev) => prev + '\n' + `Error: ${response.error}`
                    );
                }

                // Resolve the promise if there's a pending one
                if (resolveInstance) {
                    resolveInstance();
                    resolveInstance = null;
                }
                return;
            default:
                console.error(
                    `Unknown message type received from worker: ${response}`
                );
        }
    };

    workerInstance = worker;
};

// Hook for React components to use the Python worker
export const usePythonWorker = ({ setPythonOutput }: usePythonWorkerProps) => {
    useEffect(() => {
        // Initialize the worker if it hasn't been initialized yet
        if (workerInstance === null) {
            console.log('intializing python worker');
            initializePythonWorker(setPythonOutput);
        } // Otherwise, do nothing

        // Clean up worker on unmount if this is the component that created it
        return () => {
            // We don't terminate the worker here to maintain the singleton
            // The worker will persist until the page is refreshed
        };
    }, [setPythonOutput]);

    return { runPython };
};
