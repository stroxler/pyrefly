/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import * as stylex from '@stylexjs/stylex';
import typography from './typography';
interface LandingPageSectionProps {
    title?: string;
    child: React.ReactNode;
    id?: string;
    isFirstSection?: boolean;
    isLastSection?: boolean;
    hasBrownBackground?: boolean;
}

export default function LandingPageSection({
    title,
    child,
    id = '',
    isFirstSection = false,
    isLastSection = false,
}: LandingPageSectionProps): React.ReactElement {
    return (
        <section
            id={id}
            {...stylex.props(
                styles.section,
                isFirstSection ? styles.isFirstSection : null,
                isLastSection ? styles.lastSection : null
            )}
        >
            <div className="container">
                {title != null ? (
                    <h2 {...stylex.props(styles.sectionTitle, typography.h2)}>
                        {title}
                    </h2>
                ) : (
                    <></>
                )}
                {child}
            </div>
        </section>
    );
}

const styles = stylex.create({
    section: {
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'center',
        alignItems: 'center',
        flex: 1,
        position: 'relative',
        padding: '1rem 0',
    },
    isFirstSection: {
        paddingTop: '7rem',
    },
    lastSection: {
        paddingBottom: '7rem',
    },
    sectionTitle: {
        marginTop: '2rem',
        color: 'var(--color-text)',
    },
});
