/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import MonacoEditorButton from './MonacoEditorButton';
import { PyodideStatus } from './PyodideStatus';

interface RunPythonButtonProps {
    runPython: () => Promise<void>;
    pyodideStatus: PyodideStatus;
    setPyodideStatus: React.Dispatch<React.SetStateAction<PyodideStatus>>;
}

export default function RunPythonButton({
    runPython,
    pyodideStatus,
    setPyodideStatus,
}: RunPythonButtonProps): React.ReactElement {
    // Consider the button as "running" if Pyodide is either initializing or running
    const isRunning =
        pyodideStatus === PyodideStatus.INITIALIZING ||
        pyodideStatus === PyodideStatus.RUNNING;

    const setIsRunning = (running: boolean) => {
        setPyodideStatus(
            // We set the pyodide status to FINISHED_RUNNING based on the button state because we purposely want to sync the button state with
            // the pyodide status. There's a 1 second delay between when the button is clicked and when it's shown to finish running.
            running ? PyodideStatus.RUNNING : PyodideStatus.FINISHED_RUNNING
        );
    };
    return (
        <MonacoEditorButton
            id="run-python-button"
            onClick={runPython}
            defaultLabel="▶️ Run"
            runningLabel="⏳ Running..."
            ariaLabel="run Python code button"
            isRunning={isRunning}
            setIsRunning={setIsRunning}
        />
    );
}
