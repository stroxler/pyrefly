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
            <BrowserOnly>
                {() => (
                    <React.Suspense fallback={<div>Loading...</div>}>
                        <Sandbox sampleFilename={SANDBOX_FILE_NAME} />
                    </React.Suspense>
                )}
            </BrowserOnly>
        </Layout>
    );
}
