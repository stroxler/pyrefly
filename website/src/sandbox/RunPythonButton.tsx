/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, { useCallback, useEffect } from 'react';
import MonacoEditorButton from './MonacoEditorButton';
import type { editor } from 'monaco-editor';
import { usePythonWorker } from './usePythonWorker';

interface RunPythonButtonProps {
    model: editor.ITextModel | null;
    onActiveTabChange: (tab: string) => void;
    isRunning: boolean;
    setIsRunning: React.Dispatch<React.SetStateAction<boolean>>;
    setPythonOutput: React.Dispatch<React.SetStateAction<string>>;
}

export default function RunPythonButton({
    model,
    onActiveTabChange,
    isRunning,
    setIsRunning,
    setPythonOutput,
}: RunPythonButtonProps): React.ReactElement {
    const { runPython } = usePythonWorker({
        setIsRunning,
        setPythonOutput,
    });

    // Run Python code using the worker
    const runPythonCode = useCallback(async () => {
        if (!model) return;

        // Switch to output tab
        onActiveTabChange('output');
        setPythonOutput('');

        const code = model.getValue();
        await runPython(code);
    }, [model, onActiveTabChange, setPythonOutput, runPython]);

    return (
        <MonacoEditorButton
            id="run-python-button"
            onClick={runPythonCode}
            defaultLabel="▶️ Run"
            runningLabel="⏳ Running..."
            disabled={isRunning}
            ariaLabel="run Python code button"
        />
    );
}
