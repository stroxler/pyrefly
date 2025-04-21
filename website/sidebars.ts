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
    {
        type: 'category' as const,
        label: 'Introduction',
        link: {
            type: 'generated-index' as const,
        },
        description: "Never used a type system before or just new to Pyrefly? Start here!",
        items: [...fbInternalOnly(['fb/getting-started',  'fb/installation']), 'configuration'],
    },
    ...fbInternalOnly(
        [
            {
                type: 'category' as const,
                label: 'Migrating to Pyrefly',
                description: "Never used a type system before or just new to Pyrefly? Start here!",
                items: ['fb/migrating-from-mypy',  'fb/migrating-from-pyright']
            },
        ]),
    {
        type: 'doc' as const,
        id: 'learn-python-typing',
        label: 'Learn Python Typing',
    },
    ...fbInternalOnly(
        [

            {
                type: 'doc' as const,
                id: 'fb/FAQ',
                label: 'FAQ',
            },
            {
                type: 'doc' as const,
                id: 'fb/IDE',
                label: 'IDE',
            },
            {
                type: 'doc' as const,
                id: 'fb/pyrefly-upgrade',
                label: 'Pyrefly Upgrade',
            },
            {
                type: 'doc' as const,
                id: 'fb/error-suppresions',
                label: 'Error Suppressions',
            },
        ]
    ),
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
