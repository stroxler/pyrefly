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
import { useState, useEffect } from 'react';
interface LandingPageSectionProps {
    title?: string;
    child: React.ReactNode;
    id?: string;
    isFirstSection?: boolean;
    isLastSection?: boolean;
    hasBrownBackground?: boolean;
    isTitleCentered?: boolean;
}

export default function LandingPageSection({
    title,
    child,
    id = '',
    isFirstSection = false,
    isLastSection = false,
    isTitleCentered = false,
}: LandingPageSectionProps): React.ReactElement {
    const [isTitleVisible, setIsTitleVisible] = useState(false);

    useEffect(() => {
        // Apply animation for specific sections
        if (id === 'performance-comparison-section') {
            // Delay the title animation to start after the Why Pyrefly section animations
            const timer = setTimeout(() => {
                setIsTitleVisible(true);
            }, 1100); // Slightly later than why pyrefly animations (950ms)

            return () => clearTimeout(timer);
        } else if (id === 'pyrefly-video') {
            // Delay the title animation for the Pyrefly video section
            const timer = setTimeout(() => {
                setIsTitleVisible(true);
            }, 1300); // Load at 1300ms as specified

            return () => clearTimeout(timer);
        } else {
            // For other sections, make title visible immediately
            setIsTitleVisible(true);
        }
    }, [id]);

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
                    <h2
                        {...stylex.props(
                            styles.sectionTitle,
                            typography.h2,
                            (id === 'performance-comparison-section' ||
                                id === 'pyrefly-video') &&
                                styles.animatedTitle,
                            (id === 'performance-comparison-section' ||
                                id === 'pyrefly-video') &&
                                isTitleVisible &&
                                styles.titleVisible,
                            isTitleCentered && styles.centeredTitle
                        )}
                    >
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
    animatedTitle: {
        opacity: 0,
        filter: 'blur(4px)',
        transform: 'translateY(15px)',
        transition: 'all 0.7s cubic-bezier(0.34, 1.56, 0.64, 1)',
    },
    titleVisible: {
        opacity: 1,
        filter: 'blur(0px)',
        transform: 'translateY(0)',
    },
    centeredTitle: {
        textAlign: 'center',
    },
});
