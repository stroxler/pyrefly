/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import { useEffect, useCallback, useRef } from 'react';
import { WorkerResponse } from './pythonWorker';

interface usePythonWorkerProps {
    setIsRunning: React.Dispatch<React.SetStateAction<boolean>>;
    setPythonOutput: React.Dispatch<React.SetStateAction<string>>;
}

export const usePythonWorker = ({
    setIsRunning,
    setPythonOutput,
}: usePythonWorkerProps) => {
    const workerRef = useRef<Worker | null>(null);
    const resolveRef = useRef<
        ((value: void | PromiseLike<void>) => void) | null
    >(null);

    useEffect(() => {
        // Create worker instance
        const worker = new Worker(
            new URL('./pythonWorker.ts', import.meta.url),
            {
                type: 'module',
            }
        );

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
                    setIsRunning(false);

                    // Resolve the promise if there's a pending one
                    if (resolveRef.current) {
                        resolveRef.current();
                        resolveRef.current = null;
                    }
                    return;
                default:
                    console.error(
                        `Unknown message type received from worker: ${response}`
                    );
            }
        };

        workerRef.current = worker;

        // Clean up worker on unmount
        return () => {
            worker.terminate();
            workerRef.current = null;
        };
    }, []);

    const runPython = useCallback(async (code: string): Promise<void> => {
        if (!workerRef.current) return;

        setIsRunning(true);
        setPythonOutput('');

        return new Promise((resolve) => {
            // Store the resolve function for later use
            resolveRef.current = resolve;

            // Send message to worker
            workerRef.current?.postMessage(code);
        });
    }, []);

    return { runPython };
};
