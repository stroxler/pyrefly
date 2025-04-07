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

const TryPyrefly = React.lazy(() => import('../try-pyrefly/TryPyrefly'));

export const PLAYGROUND_FILE_NAME = "playground.py";

export default function TryPyreflyPage(): JSX.Element {
    const { siteConfig = {} } = useDocusaurusContext();
    return (
        <Layout
            title="Try Pyrefly: the Pyrefly Playground"
            description={siteConfig.description}
            noFooter>
            <BrowserOnly>
                {() => (
                    <React.Suspense fallback={<div>Loading...</div>}>
                        <TryPyrefly sampleFilename={PLAYGROUND_FILE_NAME} />
                    </React.Suspense>
                )}
            </BrowserOnly>
        </Layout>
    );
}
