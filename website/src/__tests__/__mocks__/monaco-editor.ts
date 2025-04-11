/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Mock implementation of monaco-editor library for testing

/**
 * Interface for editor model
 */
interface EditorModel {
    uri: { path: string };
    getValue: () => string;
    setValue: (value: string) => void;
    updateOptions: (options: any) => void;
    dispose: () => void;
    onDidChangeContent: (listener: any) => { dispose: () => void };
    getFullModelRange: () => {
        startLineNumber: number;
        startColumn: number;
        endLineNumber: number;
        endColumn: number;
    };
}

/**
 * Interface for editor instance
 */
interface EditorInstance {
    dispose: () => void;
    onDidChangeModelContent: (listener: any) => { dispose: () => void };
    getModel: () => EditorModel;
    setValue: (value: string) => void;
    getValue: () => string;
    getPosition: () => { lineNumber: number; column: number };
    setPosition: (position: { lineNumber: number; column: number }) => void;
    revealLine: (lineNumber: number) => void;
    focus: () => void;
    layout: () => void;
}

/**
 * Monaco editor mock for testing
 */
const monaco = {
    // Basic editor functionality
    editor: {
        // Model management
        createModel: jest.fn(() => ({
            uri: { path: '/mock-file.py' },
            getValue: jest.fn(() => 'mock code'),
            setValue: jest.fn(),
            updateOptions: jest.fn(),
            dispose: jest.fn(),
            onDidChangeContent: jest.fn(() => ({ dispose: jest.fn() })),
            getFullModelRange: jest.fn(() => ({
                startLineNumber: 1,
                startColumn: 1,
                endLineNumber: 1,
                endColumn: 1,
            })),
        })),
        onDidCreateModel: jest.fn((callback) => ({ dispose: jest.fn() })),
        setModelMarkers: jest.fn(),
        getModels: jest.fn(() => []),

        // Editor instance
        create: jest.fn(() => ({
            dispose: jest.fn(),
            onDidChangeModelContent: jest.fn(() => ({ dispose: jest.fn() })),
            getModel: jest.fn(() => ({
                uri: { path: '/mock-file.py' },
                getValue: jest.fn(() => 'mock code'),
                setValue: jest.fn(),
            })),
            setValue: jest.fn(),
            getValue: jest.fn(() => 'mock code'),
            getPosition: jest.fn(() => ({ lineNumber: 1, column: 1 })),
            setPosition: jest.fn(),
            revealLine: jest.fn(),
            focus: jest.fn(),
            layout: jest.fn(),
        })),

        // Editor options
        EditorOption: {
            readOnly: 'readOnly',
            fontSize: 'fontSize',
            minimap: 'minimap',
            lineNumbers: 'lineNumbers',
        },
    },

    // Languages functionality
    languages: {
        register: jest.fn(),
        setMonarchTokensProvider: jest.fn(),
        registerCompletionItemProvider: jest.fn(),
        registerDefinitionProvider: jest.fn(),
        registerHoverProvider: jest.fn(),
        registerInlayHintsProvider: jest.fn(),

        CompletionItemKind: {
            Function: 'Function',
            Constructor: 'Constructor',
            Field: 'Field',
            Variable: 'Variable',
            Class: 'Class',
            Interface: 'Interface',
            Module: 'Module',
            Property: 'Property',
            Method: 'Method',
            Keyword: 'Keyword',
            Snippet: 'Snippet',
            Text: 'Text',
        },

        CompletionItemInsertTextRule: {
            InsertAsSnippet: 1,
        },
    },

    // URI handling
    Uri: {
        parse: jest.fn((path: string) => ({ path })),
        file: jest.fn((path: string) => ({ path })),
    },

    // Range and position
    Range: jest.fn(
        (
            startLineNumber: number,
            startColumn: number,
            endLineNumber: number,
            endColumn: number
        ) => ({
            startLineNumber,
            startColumn,
            endLineNumber,
            endColumn,
        })
    ),
    Position: jest.fn((lineNumber: number, column: number) => ({
        lineNumber,
        column,
    })),

    // Markers
    MarkerSeverity: {
        Error: 8,
        Warning: 4,
        Info: 2,
        Hint: 1,
    },

    // Editor configuration
    KeyCode: {
        Enter: 3,
        Tab: 2,
        Escape: 9,
    },
};

export default monaco;
