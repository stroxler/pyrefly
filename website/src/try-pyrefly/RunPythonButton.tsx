/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, { useState, useEffect, useCallback, useRef } from 'react';
import { loadPyodide, PyodideInterface } from 'pyodide';
import MonacoEditorButton from './MonacoEditorButton';
import type { editor } from 'monaco-editor';

interface RunPythonButtonProps {
    model: editor.ITextModel | null;
    onActiveTabChange: (tab: string) => void;
    isRunning: boolean;
    setIsRunning: React.Dispatch<React.SetStateAction<boolean>>;
    pythonOutput: string;
    setPythonOutput: React.Dispatch<React.SetStateAction<string>>;
}

export default function RunPythonButton({
    model,
    onActiveTabChange,
    isRunning,
    setIsRunning,
    pythonOutput,
    setPythonOutput,
}: RunPythonButtonProps): React.ReactElement {
    const [pyodide, setPyodide] = useState<PyodideInterface | null>(null);

    // Initialize Pyodide
    useEffect(() => {
        async function initPyodide() {
            try {
                const pyodideInstance = await loadPyodide({
                    indexURL: `${window.location.origin}/pyodide`,
                });

                const updateOutput = (output: string) => {
                    setPythonOutput((prev) => prev + '\n' + output);
                };
                // Set up stdout and stderr capture
                pyodideInstance.setStdout({ batched: updateOutput });
                pyodideInstance.setStderr({ batched: updateOutput });
                setPyodide(pyodideInstance);
            } catch (error) {
                console.error('Error loading Pyodide:', error);
            }
        }

        initPyodide();
    }, []);

    // Run Python code using Pyodide
    const runPythonCode = useCallback(async () => {
        if (!pyodide || !model) return;

        // Switch to output tab
        onActiveTabChange('output');
        setIsRunning(true);
        setPythonOutput('');

        try {
            const code = model.getValue();
            pyodide.runPython(code);
        } catch (error) {
            console.error('Error running Python code:', error);
            setPythonOutput((prev) => prev + '\n' + error);
        } finally {
            setIsRunning(false);
        }
    }, [pyodide, model, onActiveTabChange]);

    return (
        <MonacoEditorButton
            id="run-python-button"
            onClick={runPythonCode}
            defaultLabel="▶️ Run"
            successLabel="⏳ Running..."
            disabled={isRunning || !pyodide}
            ariaLabel="run Python code button"
        />
    );
}
