/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import BrowserOnly from '@docusaurus/BrowserOnly';
import * as React from 'react';
import * as docusaurusTheme from '@docusaurus/theme-common';
import * as stylex from '@stylexjs/stylex';

const Sandbox = React.lazy(() => import('./Sandbox'));

interface CodeSnippetProps {
    sampleFilename: string;
    isCodeSnippet?: boolean;
    codeSample?: string;
}

export default function CodeSnippet({
    sampleFilename,
    codeSample = '',
}: CodeSnippetProps): JSX.Element {
    const { colorMode } = docusaurusTheme.useColorMode();

    if (sampleFilename == null) {
        throw "Missing sampleFilename. IDE services won't work properly.";
    }

    return (
        <pre
            {...stylex.props(colorMode === 'light' ? styles.preLightMode : {})}
        >
            <BrowserOnly>
                {() => (
                    <React.Suspense fallback={<div>Loading...</div>}>
                        <Sandbox
                            sampleFilename={sampleFilename}
                            isCodeSnippet={true}
                            codeSample={codeSample}
                        />
                    </React.Suspense>
                )}
            </BrowserOnly>
        </pre>
    );
}

const styles = stylex.create({
    preLightMode: {
        backgroundColor: '#e0e0e0', // Use darker grey in light mode
    },
});
