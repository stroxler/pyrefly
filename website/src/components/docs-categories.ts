/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import { docsSidebar } from '../../sidebars';

// Union type for all sidebar items
type SidebarItem = SidebarDocItem | SidebarCategoryItem;

// Define proper types for sidebar items
interface SidebarItemBase {
    label: string;
    description?: string;
}

// Type for doc items
interface SidebarDocItem extends SidebarItemBase {
    type: 'doc';
    id: string;
}

// Type for category items
interface SidebarCategoryItem extends SidebarItemBase {
    type: 'category';
    items: string[] | any[]; // Can be an array of strings or other sidebar items
    link?: SidebarCategoryLink;
}

// Type for category links
type SidebarCategoryLink =
    | SidebarCategoryLinkDoc
    | SidebarCategoryLinkGeneratedIndex;

// Type for document links in category items
interface SidebarCategoryLinkDoc {
    type: 'doc';
    id: string;
}

// Type for generated index links in category items
interface SidebarCategoryLinkGeneratedIndex {
    type: 'generated-index';
}

interface DocsCategory {
    docId?: string;
    href: string;
    label: string;
    type: string;
    description?: string;
}

const docsCategories: DocsCategory[] = docsSidebar.map((item: SidebarItem) => {
    const label = item.label;
    const id = item.type === 'doc' ? item.id! : item.items![0];
    let href = `/en/docs/${id}`;
    if (href.endsWith('/index')) href = href.substring(0, href.length - 6);

    if (item.type === 'category' && item.description !== undefined) {
        return { type: 'link', href, label, description: item.description };
    }
    return { type: 'link', docId: id, href, label };
});

export default docsCategories;
