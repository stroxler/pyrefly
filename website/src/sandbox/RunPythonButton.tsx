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

interface RunPythonButtonProps {
    runPython: () => Promise<void>;
    isRunning: boolean;
    setIsRunning: React.Dispatch<React.SetStateAction<boolean>>;
}

export default function RunPythonButton({
    runPython,
    isRunning,
    setIsRunning,
}: RunPythonButtonProps): React.ReactElement {
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
