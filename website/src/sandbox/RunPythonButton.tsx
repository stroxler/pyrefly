/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, { useCallback } from 'react';
import MonacoEditorButton from './MonacoEditorButton';
import type { editor } from 'monaco-editor';
import { usePythonWorker } from './usePythonWorker';

interface RunPythonButtonProps {
    model: editor.ITextModel | null;
    onActiveTabChange: React.Dispatch<React.SetStateAction<string>>;
    isRunning: boolean;
    setIsRunning: React.Dispatch<React.SetStateAction<boolean>>;
    setPythonOutput: React.Dispatch<React.SetStateAction<string>>;
}

// Reusable function to create a Python code runner
export function createRunPythonCodeFunction(
    onActiveTabChange: React.Dispatch<React.SetStateAction<string>>,
    runPython: (code: string) => Promise<void>
) {
    // Return a function that takes a model parameter
    return async (model: editor.ITextModel | null) => {
        if (!model) return;

        // Switch to output tab
        onActiveTabChange('output');

        const code = model.getValue();
        await runPython(code);
    };
}

export default function RunPythonButton({
    model,
    onActiveTabChange,
    isRunning,
    setIsRunning,
    setPythonOutput,
}: RunPythonButtonProps): React.ReactElement {
    // Initialize the Python worker
    const { runPython } = usePythonWorker({ setPythonOutput });

    const runPythonCode = useCallback(async () => {
        if (!model) return;

        // Create and immediately call the run function
        const runFunction = createRunPythonCodeFunction(
            onActiveTabChange,
            runPython
        );
        await runFunction(model);
    }, [model, onActiveTabChange]);

    return (
        <MonacoEditorButton
            id="run-python-button"
            onClick={runPythonCode}
            defaultLabel="▶️ Run"
            runningLabel="⏳ Running..."
            ariaLabel="run Python code button"
            isRunning={isRunning}
            setIsRunning={setIsRunning}
        />
    );
}
