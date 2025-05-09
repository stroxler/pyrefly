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
import { useEffect, useState } from 'react';
interface WhyPyreflyGridItemProps {
    title: string;
    content: string;
    index: number;
    startAnimation: boolean;
}

export default function WhyPyreflyGridItem({
    title,
    content,
    index,
    startAnimation,
}: WhyPyreflyGridItemProps): React.ReactElement {
    const [isVisible, setIsVisible] = useState(false);

    useEffect(() => {
        // Only start animation when parent signals it's time
        if (startAnimation) {
            // Stagger the animations based on index
            const timer = setTimeout(() => {
                setIsVisible(true);
            }, index * 80); // Reduced from 150ms to 80ms delay between each card

            return () => clearTimeout(timer);
        }
    }, [startAnimation, index]);

    return (
        <div
            {...stylex.props(
                styles.whyPyreflyCard,
                isVisible && styles.whyPyreflyCardVisible
            )}
            style={{
                // Apply dynamic delay based on index
                transitionDelay: `${index * 0.05}s`, // Reduced from 0.1s to 0.05s
            }}
        >
            <h3 {...stylex.props(typography.h5, styles.cardTitle)}>{title}</h3>
            <p {...stylex.props(styles.contentText, typography.p)}>{content}</p>
        </div>
    );
}

const styles = stylex.create({
    whyPyreflyCard: {
        padding: '1.75rem',
        background: 'var(--color-why-pyre-fly-background)',
        backdropFilter: 'blur(10px)',
        borderRadius: '8px',
        border: '1px solid rgba(255, 255, 255, 0.1)',
        boxShadow:
            '0 4px 6px var(--color-shadow), 0 1px 3px var(--color-shadow-background)',
        transition: 'all 0.8s cubic-bezier(0.34, 1.56, 0.64, 1)', // Reduced from 1.2s to 0.8s
        transform: 'rotateX(15deg) translateY(20px)',
        opacity: 0,
        filter: 'blur(6px)',
        height: '100%',
        display: 'flex',
        flexDirection: 'column',
        ':hover': {
            transform: 'translateY(-5px)',
            boxShadow:
                '0 10px 20px var(--color-shadow-hovered), 0 3px 6px var(--color-shadow)',
            background: 'var(--color-why-pyre-fly-background-hovered)',
        },
    },
    whyPyreflyCardVisible: {
        opacity: 1,
        transform: 'rotateX(0deg) translateY(0)',
        filter: 'blur(0px)',
    },
    cardTitle: {
        fontWeight: 700,
        marginBottom: '0.75rem',
        color: 'var(--color-text)',
    },
    contentText: {
        fontSize: '1rem',
        lineHeight: '1.6',
        marginBottom: '0rem',
        flex: 1,
        color: 'var(--color-text)',
    },
});
