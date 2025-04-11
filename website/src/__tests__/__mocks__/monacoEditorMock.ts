/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Mock implementation of @monaco-editor/react library for testing
import * as React from 'react';

interface EditorProps {
    value?: string;
    defaultValue?: string;
    defaultLanguage?: string;
    defaultPath?: string;
    language?: string;
    theme?: string;
    options?: {
        path?: string;
        readOnly?: boolean;
        minimap?: { enabled: boolean };
        hover?: { enabled: boolean; above: boolean };
        scrollBeyondLastLine?: boolean;
        overviewRulerBorder?: boolean;
    };
    onChange?: (value: string | undefined) => void;
    onMount?: (editor: any) => void;
    keepCurrentModel?: boolean;
    height?: number | string;
}

// Mock Editor component
const MonacoEditor = jest.fn((props: EditorProps) => {
    const {
        value,
        defaultValue,
        language,
        defaultLanguage,
        options,
        defaultPath,
    } = props;

    // Use value or defaultValue, with defaultValue taking precedence if both are provided
    const editorValue = value || defaultValue || '';
    const editorLanguage = language || defaultLanguage || 'python';
    const editorPath = options?.path || defaultPath || 'playground.py';

    // Simple mock implementation that renders a div
    return React.createElement(
        'div',
        {
            id: 'monaco-editor',
            'data-language': editorLanguage,
            'data-value': editorValue,
        },
        [
            React.createElement(
                'div',
                { key: 'editor-header' },
                `Monaco Editor (Path: ${editorPath})`
            ),
            React.createElement('textarea', {
                id: 'editor-textarea',
                key: 'editor-textarea',
                value: editorValue,
                readOnly: true,
                style: { width: '100%', height: '200px' },
            }),
        ]
    );
});

// Mock loader
const loader = {
    config: jest.fn(),
    init: jest.fn(() => Promise.resolve()),
};

// Mock useMonaco hook
const useMonaco = jest.fn(() => ({
    editor: {
        create: jest.fn(),
        setModelMarkers: jest.fn(),
    },
    languages: {
        register: jest.fn(),
        registerCompletionItemProvider: jest.fn(),
    },
}));

module.exports = {
    __esModule: true,
    default: MonacoEditor,
    loader,
    useMonaco,
};
