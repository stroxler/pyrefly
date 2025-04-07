/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import '@testing-library/jest-dom';
import { act } from 'react';
import { render } from '@testing-library/react';
import TryPyrefly, { DEFAULT_PYTHON_PROGRAM } from '../try-pyrefly/TryPyrefly';
import { PLAYGROUND_FILE_NAME } from '../pages/try';

describe('Try Pyrefly Component', () => {
    test('render sandbox correctly', async () => {
        const container = await act(async () => {
            const { container } = render(
                <TryPyrefly sampleFilename={PLAYGROUND_FILE_NAME} />,
            );

            await Promise.resolve(); // Let any promises from timers resolve
            return container;
        });

        expectMonacoEditorLoadedWithContent(
            container,
            PLAYGROUND_FILE_NAME,
            DEFAULT_PYTHON_PROGRAM,
            false,
        );

        // Run test with --update-snapshot to update the snapshot if the test is failing after
        // you made a intentional change to the home page
        expect(container).toMatchSnapshot();
    });

    test('renders in code snippet mode without error panel', async () => {
        const fileName = 'snippet.py';
        const programContent = 'def hello(): pass';
        const container = await act(async () => {
            const { container } = render(
                <TryPyrefly
                    sampleFilename={fileName}
                    isCodeSnippet={true}
                    codeSample={programContent}
                />,
            );

            await Promise.resolve(); // Let any promises from timers resolve
            return container;
        });

        expectMonacoEditorLoadedWithContent(
            container,
            fileName,
            programContent,
            true,
        );

        // Run test with --update-snapshot to update the snapshot if the test is failing after
        // you made a intentional change to the home page
        expect(container).toMatchSnapshot();
    });

    function expectMonacoEditorLoadedWithContent(
        container: HTMLElement,
        fileName: string,
        programContent: string,
        isCodeSnippet: boolean,
    ) {
        const tryEditorElement = container.querySelector('#tryPyrefly-editor');
        expect(tryEditorElement).toBeInTheDocument();

        // Verify that the code editor container is a child of tryPyrefly-editor
        const codeEditorContainer = tryEditorElement?.querySelector(
            '#tryPyrefly-code-editor-container',
        );
        expect(codeEditorContainer).toBeInTheDocument();

        // Verify that the results container is a child of tryPyrefly-editor
        const resultsContainer = tryEditorElement?.querySelector(
            '#tryPyrefly-results-container',
        );
        if (isCodeSnippet) {
            expect(resultsContainer).not.toBeInTheDocument();
        } else {
            expect(resultsContainer).toBeInTheDocument();
        }

        // Verify that the Monaco editor is rendered
        const monacoEditor =
            codeEditorContainer.querySelector('#monaco-editor');
        expect(monacoEditor).toBeInTheDocument();

        // Verify that the editor has the correct path
        expect(monacoEditor.textContent).toContain(
            `Monaco Editor (Path: ${fileName})`,
        );

        // Verify that the editor textarea is rendered
        const editorTextarea = monacoEditor.querySelector('#editor-textarea');
        expect(editorTextarea).toBeInTheDocument();

        // Verify that the editor content exactly matches the default Python program
        expect(editorTextarea).toHaveValue(programContent);

        // Verify that the share URL button exists
        const shareUrlButton =
            codeEditorContainer.querySelector('#share-url-button');
        if (isCodeSnippet) {
            expect(shareUrlButton).not.toBeInTheDocument();
        } else {
            expect(shareUrlButton).toBeInTheDocument();
        }
    }
});
