/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

/*
 * We should explicitly list all the labels below, so that src/js/docs-categories.js that generates
 * the /en/docs page will automatically work.
 *
 * For categories, the first item must be the index page.
 */

import { fbInternalOnly } from 'docusaurus-plugin-internaldocs-fb/internal';
import type { SidebarsConfig } from '@docusaurus/plugin-content-docs';

const docsSidebar = [
    // TODO (T221099224):Release getting started and installation doc pages to public
    ...fbInternalOnly([
        {
            type: 'doc' as const,
            id: 'fb/getting-started',
            label: '[Internal Only] Getting Started',
        },
        {
            type: 'doc' as const,
            id: 'fb/installation',
            label: '[Internal Only] Installation',
        },
    ]),
    {
        type: 'doc' as const,
        id: 'configuration',
        label: 'Configuration',
    },
    {
        type: 'doc' as const,
        id: 'learn-python-typing',
        label: 'Learn Python Typing',
    },
    {
        type: 'doc' as const,
        id: 'error-kinds',
        label: 'Error Kinds',
    },
    {
        type: 'doc' as const,
        id: 'import-resolution',
        label: 'Import Resolution',
    },
];

const sidebars: SidebarsConfig = {
    docsSidebar,
};

export { docsSidebar };
export default sidebars;
