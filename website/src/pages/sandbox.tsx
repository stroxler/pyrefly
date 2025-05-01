/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import BrowserOnly from '@docusaurus/BrowserOnly';
import Layout from '@theme/Layout';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import * as stylex from '@stylexjs/stylex';

const Sandbox = React.lazy(() => import('../sandbox/Sandbox'));

export const SANDBOX_FILE_NAME = 'sandbox.py';

export default function SandboxPage(): JSX.Element {
    const { siteConfig = {} } = useDocusaurusContext();
    return (
        <Layout
            title="Try Pyrefly: the Pyrefly Sandbox"
            description={siteConfig.description}
            noFooter
        >
            {
                // TODO (T222948083) Re-enable internal sandbox after we fix issues with sandbox on static docs
                process.env.INTERNAL_STATIC_DOCS === '1' ? (
                    <header {...stylex.props(styles.title)}>
                        Sandbox isn't currently available Internally, please use
                        the{' '}
                        <a
                            href="https://pyrefly.org/sandbox"
                            {...stylex.props(styles.hyperlink)}
                        >
                            {' '}
                            public sandbox
                        </a>
                    </header>
                ) : (
                    <BrowserOnly>
                        {() => (
                            <React.Suspense fallback={<div>Loading...</div>}>
                                <Sandbox sampleFilename={SANDBOX_FILE_NAME} />
                            </React.Suspense>
                        )}
                    </BrowserOnly>
                )
            }
        </Layout>
    );
}

const styles = stylex.create({
    title: {
        marginTop: '10px',
        marginLeft: '10px',
        fontSize: 32,
    },
    hyperlink: {
        textDecoration: 'underline',
        color: '#337ab7',
    },
});
