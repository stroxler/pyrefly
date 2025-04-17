/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, { useState, useEffect, useRef } from 'react';
import MonacoEditorButton from './MonacoEditorButton';
import Editor from '@monaco-editor/react';
import * as LZString from 'lz-string';
import * as stylex from '@stylexjs/stylex';
import TryPyreflyResults from './TryPyreflyResults';
import {
    monaco,
    setAutoCompleteFunction,
    setGetDefFunction,
    setHoverFunctionForMonaco,
    setInlayHintFunctionForMonaco,
} from './configured-monaco';
import type { editor } from 'monaco-editor';
import type { PyreflyErrorMessage } from './TryPyreflyResults';

export const DEFAULT_PYTHON_PROGRAM = `
from typing import *

def test(x: int):
  return f"{x}"

# reveal_type will produce a type error that tells you the type Pyrefly has
# computed for the return (in this case, str)
reveal_type(test(42))
`.trimStart();

// Import type for Pyrefly State
export interface PyreflyState {
    updateSource: (source: string) => void;
    getErrors: () => ReadonlyArray<PyreflyErrorMessage>;
    autoComplete: (line: number, column: number) => any;
    gotoDefinition: (line: number, column: number) => any;
    queryType: (line: number, column: number) => any;
    inlayHint: () => any;
}

// Lazy initialization function that will only be called when needed
export async function initializePyreflyWasm(): Promise<any> {
    const pyreflyWasmUninitializedPromise =
        typeof window !== 'undefined'
            ? import('./pyrefly_wasm')
            : new Promise<any>((_resolve) => {});

    try {
        const mod = await pyreflyWasmUninitializedPromise;
        await mod.default();
        return await mod;
    } catch (e) {
        console.log(e);
        throw e;
    }
}

// This will be used in the component
let pyreflyWasmInitializedPromise: Promise<any> | null = null;

interface TryPyreflyProps {
    sampleFilename: string;
    isCodeSnippet?: boolean;
    codeSample?: string;
}

export default function TryPyrefly({
    sampleFilename,
    isCodeSnippet = false,
    codeSample = DEFAULT_PYTHON_PROGRAM,
}: TryPyreflyProps): React.ReactElement {
    const editorRef = useRef<editor.IStandaloneCodeEditor | null>(null);
    const [errors, setErrors] =
        useState<ReadonlyArray<PyreflyErrorMessage> | null>([]);
    const [internalError, setInternalError] = useState('');
    const [loading, setLoading] = useState(true);
    const [pyreService, setPyreService] = useState<PyreflyState | null>(null);
    const [editorHeightforCodeSnippet, setEditorHeightforCodeSnippet] =
        useState<number | null>(null);
    const [model, setModel] = useState<editor.ITextModel | null>(null);

    // Only run for initial render, and not on subsequent updates
    useEffect(() => {
        setLoading(true);
        // Initialize the WebAssembly module only when the component is mounted
        if (!pyreflyWasmInitializedPromise) {
            pyreflyWasmInitializedPromise = initializePyreflyWasm();
        }

        pyreflyWasmInitializedPromise
            .then((pyrefly) => {
                setPyreService(new pyrefly.State());
                setLoading(false);
                setInternalError('');
            })
            .catch((e) => {
                setLoading(false);
                setInternalError(JSON.stringify(e));
            });
    }, []);

    // Need to add createModel handler in case monaco model was not created at mount time
    monaco.editor.onDidCreateModel((_newModel) => {
        const curModel = fetchCurMonacoModelAndTriggerUpdate(sampleFilename);
        setModel(curModel);
        forceRecheck();
    });

    // Recheck when pyre service or model changes
    useEffect(() => {
        forceRecheck();
    }, [pyreService, model]);

    function forceRecheck() {
        if (model == null || pyreService == null) return;

        setAutoCompleteFunction(model, (l: number, c: number) =>
            pyreService.autoComplete(l, c)
        );
        setGetDefFunction(model, (l: number, c: number) =>
            pyreService.gotoDefinition(l, c)
        );
        setHoverFunctionForMonaco(model, (l: number, c: number) =>
            pyreService.queryType(l, c)
        );
        setInlayHintFunctionForMonaco(model, () => pyreService.inlayHint());

        // typecheck on edit
        try {
            const value = model.getValue();
            pyreService.updateSource(value);
            const errors =
                pyreService.getErrors() as ReadonlyArray<PyreflyErrorMessage>;
            monaco.editor.setModelMarkers(
                model,
                'default',
                mapPyreflyErrorsToMarkerData(errors)
            );
            setInternalError('');
            setErrors(errors);
        } catch (e) {
            console.error(e);
            setInternalError(JSON.stringify(e));
            setErrors([]);
        }
    }

    function onEditorMount(editor: editor.IStandaloneCodeEditor) {
        const model = fetchCurMonacoModelAndTriggerUpdate(sampleFilename);
        setModel(model);

        if (isCodeSnippet) {
            setEditorHeightforCodeSnippet(
                Math.max(50, editor.getContentHeight())
            );
        }
        editorRef.current = editor;
    }

    const handleGoToDefFromErrors = (
        startLineNumber: number,
        startColumn: number,
        endLineNumber: number,
        endColumn: number
    ) => {
        const editor = editorRef.current;
        if (editor === null) {
            return;
        }
        const range = {
            startLineNumber,
            startColumn,
            endLineNumber,
            endColumn,
        };

        editor.revealRange(range);
        editor.setSelection(range);
    };

    return (
        <div id="tryPyrefly-editor" {...stylex.props(styles.tryEditor)}>
            <div
                id="tryPyrefly-code-editor-container"
                {...stylex.props(styles.codeEditorContainer)}
            >
                {getPyreflyEditor(
                    isCodeSnippet,
                    sampleFilename,
                    codeSample,
                    forceRecheck,
                    onEditorMount,
                    editorHeightforCodeSnippet
                )}
                {!isCodeSnippet && <div {...stylex.props(styles.buttonContainer)}> <MonacoEditorButton
                            id="share-url-button"
                            onClick={() => {
                                const currentURL = window.location.href;
                                return navigator.clipboard.writeText(currentURL);
                            }}
                            defaultLabel="📋 Share URL"
                            successLabel="✓ URL Copied!"
                            ariaLabel="share URL button"
                        />  </div>}
            </div>
            {!isCodeSnippet && (
                <TryPyreflyResults
                    loading={loading}
                    goToDef={handleGoToDefFromErrors}
                    errors={errors}
                    internalError={internalError}
                />
            )}
        </div>
    );
}

function updateURL(code: string): void {
    const compressed = LZString.compressToEncodedURIComponent(code);
    const newURL = `${window.location.pathname}?code=${compressed}`;
    window.history.replaceState({}, '', newURL);
}


function getCodeFromURL(): string | null {
    if (typeof window === 'undefined') return null;
    const params = new URLSearchParams(window.location.search);
    const code = params.get('code');
    return code ? LZString.decompressFromEncodedURIComponent(code) : null;
}

function fetchCurMonacoModelAndTriggerUpdate(
    fileName: string
): editor.ITextModel | null {
    const model = monaco.editor
        .getModels()
        .filter((model) => model?.uri?.path === `/${fileName}`)[0];

    if (model == null) {
        return null;
    }

    const codeFromUrl = getCodeFromURL();
    if (codeFromUrl != null && model != null) {
        model.setValue(codeFromUrl);
    }

    // Force update to trigger initial inlay hint
    model.setValue(model.getValue());

    // Ensure tab size is correctly set
    model.updateOptions({ tabSize: 4, insertSpaces: true });

    return model;
}

function isMobile(): boolean {
    return /iPhone|iPad|iPod|Android/i.test(navigator.userAgent);
}

function getPyreflyEditor(
    isCodeSnippet: boolean,
    fileName: string,
    codeSample: string,
    forceRecheck: () => void,
    onEditorMount: (editor: editor.IStandaloneCodeEditor) => void,
    editorHeightforCodeSnippet: number | null
): React.ReactElement {
    if (isCodeSnippet) {
        return (
            <Editor
                defaultPath={fileName}
                defaultValue={codeSample}
                defaultLanguage="python"
                theme="vs-light"
                onChange={forceRecheck}
                onMount={onEditorMount}
                keepCurrentModel={true}
                height={editorHeightforCodeSnippet}
                options={{
                    readOnly: isMobile(),
                    minimap: { enabled: false },
                    hover: { enabled: true, above: false },
                    scrollBeyondLastLine: false,
                    overviewRulerBorder: false,
                }}
            />
        );
    } else {
        // TODO (T217559369): Instead of manually calculating the sandbox height, we should
        // use flexbox behavior to make the sandbox height to be 75% of the screen
        // This doesn't seem to work with the monaco editor currently.
        const screenHeight = window.innerHeight;
        const navbarElement = document.querySelector(
            '.navbar'
        ) as HTMLElement | null; // Replace with your navbar selector
        const navbarHeight = navbarElement?.offsetHeight || 0;

        const sandboxHeight = ((screenHeight - navbarHeight) * 75) / 100;

        return (
            <Editor
                defaultPath={fileName}
                defaultValue={codeSample}
                defaultLanguage="python"
                theme="vs-light"
                onChange={(value) => {
                    forceRecheck();
                    if (typeof value === 'string') {
                        updateURL(value);
                    }
                }}
                onMount={onEditorMount}
                keepCurrentModel={true}
                height={sandboxHeight}
                options={{
                    minimap: { enabled: false },
                    hover: { enabled: true, above: false },
                    scrollBeyondLastLine: false,
                    overviewRulerBorder: false,
                }}
            />
        );
    }
}

/**
 * Maps PyreflyErrorMessage array to Monaco editor IMarkerData array
 */
function mapPyreflyErrorsToMarkerData(
    errors: ReadonlyArray<PyreflyErrorMessage>
): editor.IMarkerData[] {
    return errors.map((error) => ({
        startLineNumber: error.startLineNumber,
        startColumn: error.startColumn,
        endLineNumber: error.endLineNumber,
        endColumn: error.endColumn,
        message: error.message,
        severity: error.severity,
    }));
}


// Styles for TryPyrefly component
const styles = stylex.create({
    tryEditor: {
        display: 'flex',
        flexDirection: 'column',
        flex: 1,
    },
    codeEditorContainer: {
        position: 'relative',
        display: 'flex',
        overflow: 'auto',
        borderBottom: '10px',
        background: '#fff',
        height: '100%',
    },
    buttonContainer: {
        position: 'absolute',
        display: 'flex',
        flexDirection: 'row-reverse', // Buttons start from right and go left
        zIndex: 1000,
        // Mobile styles (max-width: 768px)
        '@media (max-width: 768px)': {
            bottom: '16px',
            right: '16px',
        },
        // Desktop styles (min-width: 769px)
        '@media (min-width: 769px)': {
            top: '20px',
            right: '20px',
        },
    },
});
